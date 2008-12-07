package org.hackmode.jdlx;

public class Node {
  private Column column;

  private Node left;

  private Node right;

  private Node up;

  private Node down;

  private int rowindex;

  private int colindex;

  public Node(int rowindex, int colindex) {
    this.rowindex = rowindex;
    this.colindex = colindex;
  }

  public Column getColumn() {
    return column;
  }

  public Node getDown() {
    return down;
  }

  public Node getLeft() {
    return left;
  }

  public Node getRight() {
    return right;
  }

  public int getRowindex() {
    return rowindex;
  }

  public Node getUp() {
    return up;
  }
  
  public void setColumn(Column column) {
    this.column = column;
  }
  
  public void setDown(Node down) {
    this.down = down;
  }
  
  public void setLeft(Node left) {
    this.left = left;
  }

  public void setRight(Node right) {
    this.right = right;
  }
  
  public void setRowindex(int rowindex) {
    this.rowindex = rowindex;
  }

  public void setUp(Node up) {
    this.up = up;
  }

  public String toString() {
    return String.format("[node %d,%d]", this.rowindex, this.colindex);
  }
}
