require 'ostruct'

def make_match (match, &processor)
    os = OpenStruct.new
    os.match        = match
    os.processor    = processor
    return os
end

def make_template_write_line (lines,line)
    lines << ("WriteLine '" + line + "'")
end

$match_metaprogram      = make_match (/^@@@ metaprogram\s*$/)   do |line_context, metaprogram|
                                                                    if line_context.line_no > 1 then
                                                                        failure = "%s(%d) : metaprogram line must appear at first line" %
                                                                                    [
                                                                                        line_context.file_name  ,
                                                                                        line_context.line_no    ,
                                                                                    ]
                                                                        metaprogram.failures <<  failure
                                                                    else
                                                                        line_context.is_metaprogram = true
                                                                    end
                                                                end
$match_include          = make_match (/^@@@ include\s+(?<file>\S+)\s*$/)    do |line_context, metaprogram|
                                                                                filename    = line_context.match_data["file"]
                                                                                seen_files  = line_context.seen_files
                                                                                read_metaprogram_impl filename,seen_files,metaprogram
                                                                            end
$match_preprocess_line  = make_match (/^@@@ (?<line>.*)$/)      {|line_context, metaprogram| metaprogram.preprocess_lines << line_context.match_data["line"]}
$match_template_line    = make_match (/^@@\> (?<line>.*)$/)     {|line_context, metaprogram| metaprogram.template_lines   << line_context.match_data["line"]}
$match_program_line     = make_match (/^@@\+ (?<line>.*)$/)     {|line_context, metaprogram| metaprogram.program_lines    << line_context.match_data["line"]}
$match_escaped_line     = make_match (/^\\(?<line>@@.*)$/)      do |line_context, metaprogram|
                                                                    make_template_write_line metaprogram.template_lines, line_context.match_data["line"]
                                                                end
$match_invalid_line     = make_match (/^@@.*$/)                 do |line_context, metaprogram|
                                                                    failure = "%s(%d) : Invalid start token: %s" %
                                                                                [
                                                                                    line_context.file_name  ,
                                                                                    line_context.line_no    ,
                                                                                    line_context.line       ,
                                                                                ]
                                                                    metaprogram.failures <<  failure
                                                                end
$match_normal_line      = make_match (/^(?<line>.*)$/)          do |line_context, metaprogram|
                                                                    make_template_write_line metaprogram.template_lines, line_context.match_data["line"]
                                                                end

$matches =
            [
                $match_metaprogram      ,
                $match_include          ,
                $match_preprocess_line  ,
                $match_template_line    ,
                $match_program_line     ,
                $match_escaped_line     ,
                $match_invalid_line     ,
                $match_normal_line      ,
            ]

def read_metaprogram_impl (filename, seen_files, metaprogram)

    realpath = File.realpath filename

    # Is this file already processed?
    if seen_files.key? realpath
        return
    end

    seen_files.store realpath, true

    line_context                    = OpenStruct.new
    line_context.file_name          = realpath
    line_context.line_no            = 0
    line_context.is_metaprogram     = false
    line_context.seen_files         = seen_files
    line_context.match_data         = nil
    line_context.line               = ""


    lines = []

    File.open (realpath) do |file|
        lines = file.readlines
    end

    for line in lines

        line_context.line_no   += 1
        line_context.line = line

        for match in $matches
            line_context.match_data = match.match.match line
            if line_context.match_data != nil then
                match.processor.call line_context, metaprogram
                break
            end
        end

    end

    if !line_context.is_metaprogram then
        failure = "%s(1) : No metaprogram line discovered" %
                    [
                        line_context.file_name  ,
                    ]
        metaprogram.failures <<  failure
    end

    return
end

def read_metaprogram (filename)
    seen_files = {}

    metaprogram = OpenStruct.new
    metaprogram.preprocess_lines   = []
    metaprogram.template_lines     = []
    metaprogram.program_lines      = []
    metaprogram.failures           = []

    read_metaprogram_impl filename, seen_files, metaprogram

    return metaprogram
end

def execute_metaprogram (filename)
    metaprogram = read_metaprogram filename

    puts metaprogram.preprocess_lines
    puts metaprogram.program_lines
    puts metaprogram.template_lines
end

execute_metaprogram "my.mp"
