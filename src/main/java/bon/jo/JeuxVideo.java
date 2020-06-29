package bon.jo;

import java.time.LocalDate;
import java.util.List;

public class JeuxVideo {
    private String name;
    private LocalDate sorti;
    private List<Genre> genre;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public LocalDate getSorti() {
        return sorti;
    }

    public void setSorti(LocalDate sorti) {
        this.sorti = sorti;
    }

    public List<Genre> getGenre() {
        return genre;
    }

    public void setGenre(List<Genre> genre) {
        this.genre = genre;
    }

    static class Genre{
        private String name;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }
    }
}
