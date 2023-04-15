module MyUtils

export my_count_lines, my_readline, my_read_table, my_write_table, my_format_numLen

using DelimitedFiles
using Tables: table


function my_count_lines(filePath::String)::Int64## The result may differ to `wc -l filePath`
    num_l::Int64 = 0
    ioin = open(filePath, "r")
    num_l = countlines(ioin)
    close(ioin)
    return num_l
end


function util_my_readline(filePath::String, nth::Int64, maxLen::Int64=8192*16, eol::AbstractChar='\n', windowSize::Int64=8192)::String
    aeol = UInt8(eol)
    a = Vector{UInt8}(undef, windowSize)
    nl = nb = 0
    nthd = nth - 1
    numElem = maxLen + 1
    outStrU = Vector{UInt8}(undef, numElem)
    UFilled = 0
    io = open(filePath, "r", lock=false)
    while !eof(io)
        nb = readbytes!(io, a)
        @views for i=1:nb#@simd
            @inbounds nl += a[i] == aeol
            if nl == nthd
                UFilled += 1
                outStrU[UFilled] = a[i]
            end
            if nl == nth || UFilled == numElem
                break
            end
        end
        if nl == nth || UFilled == numElem
            break
        end
    end
    close(io)
    outStr = ""
    if UFilled > 1
        if nth > 1
            outStr = join(Char.(outStrU[2:UFilled]))
        else
            outStr = join(Char.(outStrU[1:UFilled]))
        end
    else
        if nth < 2
            outStr = Char(outStrU[1]) * ""
        end
    end
    return outStr
end
##
function my_readline(filePath::String, nth::Int64; maxLen::Int64=8192*16, delim::Char='\t', isSplit::Bool=true, nElem::Int64=0, eol::AbstractChar='\n', windowSize::Int64=8192)::Union{String, Vector{String}}
    outStr = util_my_readline(filePath, nth, maxLen, eol, windowSize)
    if isSplit && length(outStr) > 1
        o_split::Vector{String} = split(outStr, delim)
        if nElem > 0
            return o_split[1:nElem]
        else
            return o_split
        end
    else
        return outStr
    end
end


function my_read_table(XPath::String, type::DataType=Float32, delim::Char=',', isTranspose::Bool=false)
    txt = open(XPath) do file; read(file, String); end;
    out = readdlm(IOBuffer(txt), delim, type)
    if isTranspose; out = transpose(out) |> Matrix; end;
    return out
end


function my_write_table(tableO, XPath::String; delim::Char='\t', isAppend::Bool=false, toTable::Bool=false)
    if isAppend
        isapd = "a"
    else
        isapd = "w"
    end
    if toTable
        open(XPath, isapd) do io
            writedlm(io, table(tableO), delim)
        end
    else
        open(XPath, isapd) do io
            writedlm(io, tableO, delim)
        end
    end
    return nothing
end


function my_format_numLen(InNum::Int64, lenO::Int64)::String
    ns = string(InNum)
    while length(ns) < lenO
        ns = string("0", ns)
    end
    return ns
end


end
