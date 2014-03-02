
class Document
    def initialize ()
        @current_line   = ""
        @lines          = []
    end

    def write (value)
        @current_line << (value || "").to_s
    end

    def new_line ()
        @lines << @current_line
        @current_line = ""
    end

    def get_lines ()
        return @lines
    end

end
