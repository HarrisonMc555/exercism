
import java.util.List;

public class BST<T> {

    private Node<T> root;

    public static class Node<TT> {

        protected Node<TT> left;
        protected Node<TT> right;
        protected TT data;

        public Node(TT data) {
            this.left = null;
            this.left = null;
            this.data = data;
        }

        public TT getData() {
            return this.data;
        }

        public Node<TT> getLeft() {
            return this.left;
        }

        public Node<TT> getRight() {
            return this.right;
        }

    }

    public BST() {
        this.root = null;
    }

    public Node<T> getRoot() {
        return this.root;
    }

    public void insert(T data) {
        this.root = new Node<T>(data);
    }

    public List<Integer> getAsLevelOrderList() {
        return null;
    }

    public List<Integer> getAsSortedList() {
        return null;
    }

}
