package bon.jo;

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;

public class JeuxVideo2 {
    private String name;
    private LocalDate sorti;
    private List<Genre2> genre;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        JeuxVideo2 jeuxVideo = (JeuxVideo2) o;
        return Objects.equals(name, jeuxVideo.name) &&
                Objects.equals(sorti, jeuxVideo.sorti) &&
                Objects.equals(genre, jeuxVideo.genre);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, sorti, genre);
    }

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

    public List<Genre2> getGenre() {
        return genre;
    }

    public void setGenre(List<Genre2> genre) {
        this.genre = genre;
    }

    public static class Genre2 {
        private String name;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Genre2 genre = (Genre2) o;

            return Objects.equals(name, genre.name);
        }

        @Override
        public int hashCode() {
            return name != null ? name.hashCode() : 0;
        }
    }
}
