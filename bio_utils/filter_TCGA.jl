# Filter TCGA, paralleled.
using CSV, Statistics, Tables, DataFrames, DelimitedFiles
#using StatsBase: UnitRangeTransform, transform, fit

## Main

### Filter samples: with 3 omics? in tags? (line2 removed) (sorted by id_Xomics)
function filter_cols(origFilePaths::Vector{String}, tags::Vector{String}, grepLen=16, delim="\t")
    id_Xomics = sampl_own_Xomics(origFilePaths, grepLen, delim, false)
    Threads.@threads for nf in eachindex(origFilePaths)
        filter_cols_byfile(origFilePaths[nf], tags, id_Xomics, delim)
    end
end

### Filter features by threshold NA and threshold Var, paralleled.
function filter_rows(filePath::String, threshold_var=0.001, delim="\t", threshold_NA=0.25)
    fileDir = string(dirname(filePath), "/")
    fileName = basename(filePath)
    #new_dir = fileDir#[1:end-length(split(fileDir, "/")[end-1]) - 1]
    #### Slice for parallel
    ##### Count row
    num_row = parse(Int64, split(read(`wc -l $filePath`, String), " ")[1]) - 2 ## remove 2-row headers
    @show num_row
    threadNum = Threads.nthreads()
    pLen = num_row / threadNum |> floor
    startPts = [] |> Vector{Int64}
    endPts = [] |> Vector{Int64}
    for nT in 1:threadNum
        append!(startPts, Int64(1 + (nT - 1) * pLen))
        append!(endPts, Int64(nT * pLen))
    end
    endPts[threadNum] = num_row
    #
    @show threadNum
    #### Calc vars, multi-thread
    Threads.@threads for nT in 1:threadNum
        calc_var_A2Blines(filePath,
                          string(fileDir, ".tmp_running_THvar-", threshold_var, "_THna-", threshold_NA, "_p-", (2 + startPts[nT]), "_", fileName),
                          (2 + startPts[nT]), (2 + endPts[nT]),  threshold_var, threshold_NA, delim)
    end
    #### Combine
    write_path = string(fileDir, "_f-X_", fileName, "_THvar-", threshold_var, "_THna-", threshold_NA, ".txt")
    tmp_path = string(fileDir, ".tmpCombn_", fileName, "_THvar-", threshold_var, "_THna-", threshold_NA, ".txt")
    f_to_grep = string(fileDir, ".tmp_*")
    sh_path = string(fileDir, ".sh_", fileName, "_THvar-", threshold_var, "_THna-", threshold_NA, ".sh")
    #### Write header
    write_csvRows(string(fileDir, ".tmp_0000_header_", fileName, "_THvar-", threshold_var, "_THna-", threshold_NA, ".txt"), read_Xth_line(filePath, delim), false, false)
    ####
    tmp_sh = string("cat ", f_to_grep, " > ", tmp_path, "\n",
                    "rm ", f_to_grep, "\n",
                    "mv ", tmp_path, " ", write_path, "\n")
    io = open(sh_path, "w"); write(io, tmp_sh); close(io);
    run(`sh $sh_path`); run(`rm $sh_path`)
end

  
## Utils

function read_Xth_line(filePath::String, delim="\t", whichLine::Int64=1)
    pn = "p"
    out = split(read(`sed -n $whichLine$pn $filePath`, String), delim[1])
    out[end] = out[end][1:end-1]
    return out
end

function my_read_csv(Xdir::String, type=Float32, delm=',')
    txt = open(Xdir) do file
        read(file, String)
    end
    out = readdlm(IOBuffer(txt), delm[1], type)
    return out
end

function fill_NA(vecin::Vector{String})
    vecNums = [] |> Vector{Float64}
    vecNew = zeros(Float64, length(vecin))
    for nx in eachindex(vecin)
        if vecin[nx] == "NA"
            continue
        else
            push!(vecNums, parse(Float64, vecin[nx]))
        end
    end
    meann = mean(vecNums)
    minn = minimum(vecNums, init=Inf)
    rangen = maximum(vecNums, init=-1000) - minn
    patchn = (meann - minn) / rangen
    ## Fill
    Threads.@threads for nx in eachindex(vecin)
        if vecin[nx] == "NA"
            vecNew[nx] = patchn
        else
            vecNew[nx] = parse(Float64, vecin[nx])
        end
    end
    return reshape(vecNew, 1, size(vecNew)[1])
end

### Q: Whose var?
function Var_minmax_normalize(vecin::Vector, vec_noNA::Vector{Float64}, containNA=false, returnVar=true)
    vecin = string.(vecin)
    vecin = parse.(Float64, vecin)
    if containNA
        meann = mean(vec_noNA)
        minn = minimum(vec_noNA, init=Inf)
        rangein = maximum(vec_noNA, init=-1000) - minn
        patchn = (meann-minn)/rangein
    else
        minn = minimum(vecin)
        rangein = maximum(vecin) - minn
    end
    for num_g in eachindex(vecin)
        if containNA && vecin[num_g] == -1
            vecin[num_g] = patchn
        else
            vecin[num_g] = (vecin[num_g] - minn) / rangein
        end
    end
    if returnVar
        return var(vecin), vecin
    else return vecin
    end
end

function write_csvRows(filename::String, strIn::Vector, isAppend=false, lineFeed=true, delim="\t")
    if lineFeed
        tmp_str = "\n"
    else tmp_str = ""
    end
    for tt in 1:length(strIn)
        if tt==1
            tmp_str = string(tmp_str, strIn[tt])
        else tmp_str = string(tmp_str, delim[1], strIn[tt])
        end
    end
    if isAppend
        io = open(filename, "a")
    else io = open(filename, "w")
    end
    write(io, tmp_str)
    close(io)
end


### Pick samples' id owning meth, mirna and mrna
function sampl_own_Xomics(origFilePaths::Vector{String}, len_id::Int64=16, delim="\t", isWrite=false, wrt_pref::String=".ownAll_")
    out = [] |> Vector{String}
    for fpath in eachindex(origFilePaths)
        if fpath < 2
            out = read_Xth_line(origFilePaths[fpath], delim)[Not(1)]
            if len_id > 1
                out = cut_vec_str(out, len_id)
            end
            continue
        end
        ## If fpath > 1
        tcgaB = read_Xth_line(origFilePaths[fpath], delim)[Not(1)]
        if len_id > 1
            tcgaB = cut_vec_str(tcgaB, len_id)
        end
        out = intersect(unique(out), unique(tcgaB))
    end
    if isWrite
        CSV.write(string(wrt_pref, "_len_", len_id, ".csv"), Tables.table(out), header=false)
    end
    return out
end


function filter_cols_byfile(origFilePath::String, tags::Vector{String}, id_Xomics::Vector{String}, delim="\t")
    grepLen = length(id_Xomics[1])
    line1 = read_Xth_line(origFilePath, delim)
    cols_inTags = read_Xth_line(origFilePath, delim, 2) .∈ (tags,)
    cols_Xomics = cut_vec_str(line1, grepLen) .∈ (id_Xomics,)
    avail_cols  = (cols_inTags + cols_Xomics) .> 1
    line1_avail = line1[avail_cols]
    println("Available col num: ", sum(avail_cols))
    ## sort cols
    col_sort = [] |> Vector{Int64}
    for idom in eachindex(id_Xomics)
        push!(col_sort, findfirst(x -> x == id_Xomics[idom], cut_vec_str(line1_avail, grepLen)))
    end
    ## header
    o1 = [string(line1[1])]
    append!(o1, line1_avail[col_sort])
    write_csvRows(string(dirname(origFilePath), "/", ".fltCol_", basename(origFilePath)), o1, false, false, delim)
    ##
    for rown in CSV.Rows(origFilePath, delim=delim[1], header=2)
        rown = string.(rown)
        out = [string(rown[1])]
        append!(out, rown[avail_cols][col_sort])
        write_csvRows(string(dirname(origFilePath), "/", ".fltCol_", basename(origFilePath)), out, true, true, delim)
    end
end


function procc_RowsElem(rowswh::String, tmp_row::Vector{Float64}, tmp_noNA::Vector{Float64})
    pp_spl = split(rowswh, "e")
    if rowswh == "NA"
        pp = -1 |> Float64
    ## If num is too little, e.g. 1e-16
    elseif length(pp_spl) > 1 && parse(Int64, pp_spl[2]) < -15
        pp = 0 |> Float64
        append!(tmp_noNA, pp)
    else
        pp = parse(Float64, rowswh)
        append!(tmp_noNA, pp)
    end
    append!(tmp_row, pp)
    return tmp_row, tmp_noNA
end

function procc_byRow(oneRow::Vector{String})
    tmp_row = [] |> Vector{Float64}
    tmp_noNA = [] |> Vector{Float64}
    for elm in eachindex(oneRow)
        tmp_row, tmp_noNA = procc_RowsElem(string(oneRow[elm]), tmp_row, tmp_noNA)
    end
    cNA = false
    if length(tmp_noNA) < length(tmp_row); cNA = true; end;
    return tmp_row, tmp_noNA, cNA
end

function calc_var_A2Blines(filePath::String, writePath::String, lineBegin::Int64, lineEnd::Int64,
                           threshold_var=0.001, threshold_NA=0.25, delim="\t")
    rowN = lineBegin
    mark_new = true
    for row in CSV.Rows(filePath, delim=delim[1], header=(lineBegin-1))
        if rowN > lineEnd; break; end;
        row = string.(row) |> Vector{String}
        tmp_row, tmp_noNA, cNA = procc_byRow(row[Not(1)])
        if (1 - threshold_NA) * (length(row) - 1) > length(tmp_noNA)
            rowN += 1
            continue
        end
        varN, vecFilled = Var_minmax_normalize(tmp_row, tmp_noNA, cNA)
        if threshold_var < varN
            out = [string(row[1])]
            append!(out, string.(vecFilled))
            if !mark_new
                write_csvRows(writePath, out, true, true, delim)
            else
                write_csvRows(writePath, out, false, true, delim)
                mark_new = false
            end
        end
        rowN += 1
    end
end

function cut_vec_str(vec_str::Vector, len::Int64=15)
    out = [] |> Vector{String}
    for strN in eachindex(vec_str)
        append!(out, [vec_str[strN][1:len]])
    end
    return out
end
