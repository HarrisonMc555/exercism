
public class BST<T> {

    private Node root;

    public class Node {

        protected Node left;
        protected Node right;
        protected T data;

        public Node(T data) {
            this.left = null;
            this.left = null;
            this.data = data;
        }

        public T getData() {
            return this.data;
        }

    }

    public BST() {
        this.root = null;
    }

    public Node getRoot() {
        return this.root;
    }

    public void insert(T data) {
        root = new Node(data);
    }

}
