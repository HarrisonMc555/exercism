
import java.util.List;
import java.util.ArrayList;

public class BST<T extends Comparable<T>> {

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
        if (root == null) {
            this.root = new Node<T>(data);
        } else {
            this.insert(root, data);
        }
    }

    private void insert(Node<T> node, T data) {
        if (data.compareTo(node.getData()) > 0) {
            if (node.getRight() == null) {
                node.right = new Node<T>(data);
            } else {
                this.insert(node.getRight(), data);
            }
        } else {
            if (node.getLeft() == null) {
                node.left = new Node<T>(data);
            } else {
                this.insert(node.getLeft(), data);
            }
        }
    }

    public List<T> getAsLevelOrderList() {
        List<T> list = new ArrayList<T>();
        List<Node<T>> nodes = new ArrayList<Node<T>>();
        nodes.add(root);
        while (!nodes.isEmpty()) {
            List<Node<T>> nextNodes = new ArrayList<Node<T>>();
            for (Node<T> node : nodes) {
                if (node == null) {
                    continue;
                }
                list.add(node.getData());
                nextNodes.add(node.getLeft());
                nextNodes.add(node.getRight());
            }
            nodes = nextNodes;
        }
        return list;
    }

    private void addToLevelOrderList(Node<T> node, List<T> list) {
        if (node == null) {
            return;
        }
        list.add(node.getData());
        this.addToLevelOrderList(node.getLeft(), list);
        this.addToLevelOrderList(node.getRight(), list);
    }

    public List<T> getAsSortedList() {
        List<T> list = new ArrayList<T>();
        this.addToSortedList(this.root, list);
        return list;
    }

    private void addToSortedList(Node<T> node, List<T> list) {
        if (node == null) {
            return;
        }
        this.addToSortedList(node.getLeft(), list);
        list.add(node.getData());
        this.addToSortedList(node.getRight(), list);
    }

}
