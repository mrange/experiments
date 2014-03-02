
class Document
    @current_line   = ""
    @lines          = []

    def write (value)
        @current_line << (value || "")
    end

    def new_line ()
        @lines << @current_line
        @current_line = ""
    end

    def get_lines ()
        return @lines
    end

end
