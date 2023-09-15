import java.awt.Font;

class nodedata {
	String title;
	Object Tag;
	Font font;
	
	public nodedata(Object sign, String t, Font f) {
		Tag = sign;
		title = t;
		font = f;
	}

	public Object getTag() {
		return Tag;
	}

	public String toString() { 
		return title;
	}

	public Font getFont() {
		return font;
	}
}