package bon.jo;

import javax.swing.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class JeuxVideo {
    private String name;
    private LocalDate sorti;
    private List<Genre> genre;
    private Integer rate;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        JeuxVideo jeuxVideo = (JeuxVideo) o;
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

    public List<Genre> getGenre() {
        return genre;
    }

    public void setGenre(List<Genre> genre) {
        this.genre = genre;
    }

    public Integer getRate() {
        return rate;
    }

    public void setRate(Integer rate) {
        this.rate = rate;
    }

    public static class Genre{
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

            Genre genre = (Genre) o;

            return Objects.equals(name, genre.name);
        }

        @Override
        public int hashCode() {
            return name != null ? name.hashCode() : 0;
        }
    }

    public static void main(String[] args) throws ClassNotFoundException {

        JeuxVideo j = new JeuxVideo();
        List<JeuxVideo.Genre> genre = new ArrayList<>();
        genre.add(new JeuxVideo.Genre());
        (genre.get(0)).setName("Action");
        genre.add(new JeuxVideo.Genre());
        (genre.get(1)).setName("Aventure");
        j.setGenre(genre);
        j.setRate(4);
        j.setName("test");
        j.setSorti(LocalDate.now());
        JPanel jPanel = ReflectUtil.create(JeuxVideo.class).toView(j);
        JFrame jf =  new JFrame();
        jf.getContentPane().add(jPanel);
        jf.pack();
        jf.setVisible(true);
        jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
}
