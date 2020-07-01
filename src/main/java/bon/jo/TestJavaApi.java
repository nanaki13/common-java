package bon.jo;

import java.util.ArrayList;
import java.util.List;

public class TestJavaApi {
    public static void main(String[] args) {
        ReflectUtil<JeuxVideo> ref = ReflectUtil.create(JeuxVideo.class);

        JeuxVideo j = new JeuxVideo();
        List<JeuxVideo.Genre> genre = new ArrayList<>();
        genre.add(new JeuxVideo.Genre());
        (genre.get(0)).setName("Action");
        j.setGenre(genre);
        j.setName("test");
        System.out.println(ref.asFullMap(j));
    }
}
