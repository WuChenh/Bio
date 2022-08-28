## Support multi-trait prediction by genotype and climate
using DelimitedFiles, Random, Statistics, RData, Clustering, Flux, CUDA, BSON, CSV, Tables
using StatsBase: ZScoreTransform, transform, fit
#using DataFrames, Shuffle
#push!(DL_LOAD_PATH, "/usr/local/cuda/")

function index_trait(trait_names=String["SW", "SV"])
    traits_all = String["FLL", "FLW", "PH", "SN", "FP", "PF", "SL", "SW", "SV", "SSA", "AC"]
    traits = sort(unique(trait_names))
    indi = Int64[]
    for tin in eachindex(traits)
        indi_tmp = findfirst(x -> x == traits[tin], traits_all)
        if length(indi_tmp) == 1; push!(indi, indi_tmp); end
    end
    return indi
end

function load_wt(tra_names=String["AC", "PH"], split_n=1)
    wt_traits = load("/home/chen/my/GS/ANN/wt_traits.RData")
    for tra in eachindex(tra_names)
        if tra == 1
            wt_vec = wt_traits["wt_traits"][tra_names[tra]][split_n, :]
        else
            wt_vec += wt_traits["wt_traits"][tra_names[tra]][split_n, :]
        end
    end
    wt_vec = wt_vec / length(tra_names)
    return wt_vec
end

function form_wt(wt_vec, radius=4e-6, min_clst_n=1000, is_wt=true)
    wts = hcat(zeros(length(wt_vec)), wt_vec) |> transpose
    cls = dbscan(wts, radius)
    while length(cls) < min_clst_n
        radius = 0.95 * radius
        cls = dbscan(wts, radius)
    end
    !is_wt && return ones(length(cls), length(wt_vec))
    mx = zeros(Float32, length(cls), length(wt_vec))
    Threads.@threads for clu in 1:size(mx)[1]
        act_link = vcat(cls[clu].boundary_indices, cls[clu].core_indices)
        mx[clu, act_link] .= wt_vec[act_link]
    end
    return mx
end

## Standardization
function std_by_row(mx)
    dt = fit(ZScoreTransform, mx, dims=2)
    return transform(dt, mx)
end

function my_read_csv(Xdir, type=Float32)
    txt = open(Xdir) do file
        read(file, String)
    end
    out = readdlm(IOBuffer(txt), ',', type)
    return out
end

function my_split(num=500, seed=1234, ratio_A=0.9)
    shuffled_indices = Random.shuffle(MersenneTwister(seed), 1:num)
    idx_div = round(Int, ratio_A * num)
    indices_A = shuffled_indices[1:idx_div]
    indices_B = shuffled_indices[(idx_div + 1):end]
    return indices_A, indices_B
end

function get_dataloader(traits_indi, ratio_trn=0.8, batch_size=32, Nth_rd=1, man_split=true, seed=1234, get_val=false, clims=false)
    path_f = "/home/chen/my/GS/csv/"
    x_g = my_read_csv(string(path_f, "compl_p11_g_pure.csv")) |> transpose
    x_c = my_read_csv(string(path_f, "compl_p11_e_pure.csv")) |> transpose |> std_by_row#[:, clims]
    y = my_read_csv(string(path_f, "compl_p11_p_pure.csv"))[:, traits_indi] |> transpose |> std_by_row
    if man_split
        indices_trn = my_read_csv(string(path_f, "split_trn.csv"), Int64)[Nth_rd, :]
        indices_tst = my_read_csv(string(path_f, "split_tst.csv"), Int64)[Nth_rd, :]
    else## Shuffle
        indices_trn, indices_tst = my_split(size(y)[2], seed, ratio_trn)
    end
    #if get_val
    #    indices_trn, indices_val = my_split(length(indices_trn), seed, 0.8)
    #end
    ## Count
    num_trn, num_tst = length(indices_trn), length(indices_tst)
    println("Trn: $num_trn, Tst: $num_tst")
    ## Make loaders
    if clims
        trn_loader = Flux.DataLoader((view(x_g, :, indices_trn), view(x_c, :, indices_trn), view(y, :, indices_trn)),
                                     batchsize = batch_size, parallel=true, shuffle = true)
        tst_loader = Flux.DataLoader((view(x_g, :, indices_tst), view(x_c, :, indices_tst), view(y, :, indices_tst)),
                                     batchsize = batch_size)
    else
        trn_loader = Flux.DataLoader((view(x_g, :, indices_trn), zeros(Float32, size(x_c)[1], num_trn), view(y, :, indices_trn)),
                                     batchsize = batch_size, parallel=true, shuffle = true)
        tst_loader = Flux.DataLoader((view(x_g, :, indices_tst), zeros(Float32, size(x_c)[1], num_tst), view(y, :, indices_tst)),
                                     batchsize = batch_size)
    end
    return trn_loader, tst_loader, size(y)[1], size(x_g)[1], size(x_c)[1]
end

function my_loss_Cor(dataloader, model, return_cor=false, mean_loss=false, is_gpu=false)
    #model = cpu(model) ## Reduce GPU mem usage?
    num_batch, all_pred, all_y = 1, 0, 0
    for (x_g, x_c, y) in dataloader
        #if is_gpu
            x_g, x_c = gpu(x_g), gpu(x_c)
        #end
        md_tmp = model((x_g, x_c)) |> cpu
        if num_batch == 1
            all_pred, all_y = md_tmp, y
        elseif num_batch > 1
            all_pred, all_y = hcat(all_pred, md_tmp), hcat(all_y, y)
        end
        num_batch += 1
    end
    loss, cors = zeros(Float32, size(all_y)[1]), zeros(Float32, size(all_y)[1])
    Threads.@threads for trt in eachindex(loss)
        loss[trt] = Flux.mse(view(all_pred, trt, :), view(all_y, trt, :))
        cors[trt] = cor(view(all_pred, trt, :), view(all_y, trt, :))
    end
    if return_cor; return loss, cors;
    elseif mean_loss; return mean(loss);
    else; return loss; end
end

function f_suffix(n_sp=1::Int64, is_wt=true, n_clim=0::Int64, n_cls=1000::Int64, traits=String["AC"], trait=1::Int64)
    if is_wt
        wt = 1
    else wt = 0
    end
    if length(traits) > 1
        if trait > 0
            sfx = string("tra-", traits[trait], "-", join(traits, "+"), "_clm-", n_clim, "_sp-", n_sp, "_wt-", wt, "_cls-", n_cls)
        else
            sfx = string("tra-", join(traits, "+"), "_clm-", n_clim, "_sp-", n_sp, "_wt-", wt, "_cls-", n_cls)
        end
    elseif length(traits) == 1
        sfx = string("tra-", traits[1], "_clm-", n_clim, "_sp-", n_sp, "_wt-", wt, "_cls-", n_cls)
    end
    return sfx
end

function write_rec(recs, n_sp=1::Int64, is_wt=true, n_clim=0::Int64, n_cls=1000::Int64, traits=String["AC"], prefx="")
    path_wr = "./rslt/"
    n_tra = length(traits)
    n_ep = size(recs)[1] / n_tra
    Threads.@threads for tra in 1:n_tra
        wr = zeros(0, 4)
        for nep in 1:n_ep
            slc = zeros(1,4)
            slc[1,:] .= recs[Int64((n_tra * (nep - 1)) + tra), :]
            wr = vcat(wr, slc)
        end
        suffix = f_suffix(n_sp, is_wt, n_clim, n_cls, traits, tra)
        wr_name = string(prefx, "RecTrn_", suffix, ".csv")
        CSV.write(string(path_wr, wr_name), Tables.table(wr, header=String["MSE_trn", "MSE_tst", "r_trn", "r_tst"]))
    end
end

function calc_rec(trn_loader, tst_loader, model, is_gpu=false)
    trn_mse, trn_cor = my_loss_Cor(trn_loader, model, true, is_gpu)
    tst_mse, tst_cor = my_loss_Cor(tst_loader, model, true, is_gpu)
    rec = zeros(0, 4)
    for tra in eachindex(tst_mse)
        newl = zeros(Float32, 1, 4)
        newl[1,:] .= [trn_mse[tra], tst_mse[tra], trn_cor[tra], tst_cor[tra]]
        rec = vcat(rec, newl)
    end
    return rec
end

function build_model_seq(n_tra::Int64)
    return Chain(Dense(36901, 512, sigmoid),
                 Dense(512, 128, sigmoid),
                 Dense(128, n_tra))
end

function build_model_mimo(n_tra = 2::Int64, n_cli = 19::Int64, snp_wt = ones(1024,36901))
    wt_layer = Dense(snp_wt, false)  # using BLR/RR-BLUP weight
    geno_in = Chain(wt_layer, Dense(size(snp_wt)[1] => 500)) #Dense(1000 => 500)
    clim_in = Chain(Dense(n_cli => 16), Dense(16 => 12))
    model = Chain(Parallel(vcat, geno_in, clim_in), Dense((500+12) => 256), Dense(256 => 256, sigmoid), Dense(256 => 128, sigmoid), Dense(128 => n_tra))
    return model
end

function build_model_mimo1(n_tra = 2::Int64, n_cli = 19::Int64, snp_wt = ones(1024,36901))
    wt_layer = Dense(snp_wt, false)
    geno_in = Chain(wt_layer, Dense(size(snp_wt)[1] => 300))
    clim_in = Chain(Dense(n_cli => 16), Dense(16 => 12))
    model = Chain(Parallel(vcat, geno_in, clim_in), Dense((300+12) => 128, sigmoid), Dense(128 => 128, sigmoid), Dense(128 => n_tra))
    return model
end


function my_train(model_f=build_model_mimo, traits_names=String["SW", "SV"], trn_ratio=0.8, batch_size=32, is_wt=true, Nth_rd=1, epochs_max=3000, 
                  save_model=false, min_cls=1000, seed=1234, clims=false, es_delay=30, prefx="", save_rec=true)
    traits_names = sort(unique(traits_names))
    indi_tra = index_trait(traits_names)
    trn_loader, tst_loader, n_tra, n_snp, n_clim = get_dataloader(indi_tra, trn_ratio, batch_size, Nth_rd, true, seed, false, clims)
    wts = form_wt(load_wt(traits_names, Nth_rd), 4e-6, min_cls, is_wt)
    n_cls = size(wts)[1]
    @show n_cls
    model_c = model_f(n_tra, n_clim, wts) ## Which model?
    model = model_c |> gpu
    if !clims; n_clim = 0; end
    ps = Flux.params(model)
    opt = Flux.ADAM()
    loss(x_g, x_c, y) = Flux.Losses.mse(model((x_g, x_c)), y)
    global  recs = zeros(0, 4)
    global n_ep = 0
    ## Early stopping
    es = let f = () -> my_loss_Cor(trn_loader, model, false, true, true) ## Using trn_loader due to lack of validation
        Flux.early_stopping(f, es_delay; init_score=f())
    end
    @show traits_names
    ## Train
    Flux.@epochs epochs_max begin
        for (x_g, x_c, y) in trn_loader #CuIterator(train_loader)
            x_g, x_c, y = gpu(x_g), gpu(x_c), gpu(y)
            gradients = gradient(() -> loss(x_g, x_c, y), ps)
            Flux.Optimise.update!(opt, ps, gradients)
        end
        tmp_rec = calc_rec(trn_loader, tst_loader, model, true)
        global recs = vcat(recs, tmp_rec)
        global n_ep += 1
        println("=== Cor tst: ", tmp_rec[:, 4])
        println("=== MSE tst: ", tmp_rec[:, 2])
        println("=== MSE trn: ", tmp_rec[:, 1])
        es() && break
    end
    #@show CUDA.memory_status()
    n_ep < epochs_max && n_ep > es_delay && (global n_ep = n_ep - es_delay)
    println("Number of epoch: ", n_ep, " \n")
    recs = recs[1:(n_ep * n_tra), :]
    ## Save records
    if save_rec
        write_rec(recs, Nth_rd, is_wt, n_clim, n_cls, traits_names, prefx)
    end
    ## Save model
    if save_model
        model_c = model |> cpu
        CUDA.reclaim()# model = nothing; GC.gc(true)
        save_md_name = string("./rslt/", prefx, "Model_", f_suffix(Nth_rd, is_wt, n_clim, n_cls, traits_names, 0), ".bson")
        BSON.@save save_md_name model = model_c
        run(`xz -9vk --threads=12 $save_md_name`)
        run(`rm $save_md_name`)
    else CUDA.reclaim()
    end
    return recs[(size(recs)[1] - n_tra + 1):end, 2], recs[(size(recs)[1] - n_tra + 1):end, 4] ## Tst MSE & Cor
end

# tar cvf xxx.tar *_s/
# xz -9vk --threads=14 xxx.tar
# rm xxx.tar

function my_train_reps(num_rep=10, md_f=build_model_mimo1, traits_names=String["SW", "SV"],
                       trn_ratio=0.8, batch_size=32, is_wt=true, epochs_max=3000,
                       save_model=false, min_cls=1000, seed=1234, clims=false, es_delay=30, prefx="", save_rec=true)
    rslt_MSE = zeros(Float32, num_rep, length(traits_names))
    rslt_Cor = zeros(Float32, num_rep, length(traits_names))
    for NRep in 1:num_rep
        @show NRep
        tst_mse, tst_r = my_train(md_f, traits_names, trn_ratio, batch_size, is_wt, NRep, epochs_max,
                                  save_model, min_cls, seed, clims, es_delay, prefx, save_rec)
        rslt_MSE[NRep, :] .= tst_mse
        rslt_Cor[NRep, :] .= tst_r
    end
    println("\n======================== MSE of test ========================")
    println(mean(rslt_MSE, dims=1))
    println("\n======================== Cor of test ========================")
    println(mean(rslt_Cor, dims=1), "\n")
    traits_names = sort(unique(traits_names))
    if !clims; n_clim = 0; else n_clim = 1; end
    CSV.write(string("Rslt_MSE_", f_suffix(-1, is_wt, n_clim, min_cls, traits_names, -1), ".csv"), Tables.table(rslt_MSE, header=traits_names))
    CSV.write(string("Rslt_Cor_", f_suffix(-1, is_wt, n_clim, min_cls, traits_names, -1), ".csv"), Tables.table(rslt_Cor, header=traits_names))
    return rslt_MSE, rslt_Cor
end
