
import java.util.Scanner;

public class Main {

    static Scanner scanner = new Scanner(System.in);

    // Complex number
    static class Complex {
        double real;
        double imaginary;

        Complex(double real, double imaginary) {
            this.real = real;
            this.imaginary = imaginary;
        }

        Complex add(Complex other) {
            return new Complex(
                    real + other.real,
                    imaginary + other.imaginary
            );
        }

        Complex subtract(Complex other) {
            return new Complex(
                    real - other.real,
                    imaginary - other.imaginary
            );
        }

        Complex multiply(Complex other) {
            return new Complex(
                    real * other.real - imaginary * other.imaginary,
                    real * other.imaginary + imaginary * other.real
            );
        }

        Complex divide(Complex other) {
            double denominator =
                    other.real * other.real
                            + other.imaginary * other.imaginary;

            if (Math.abs(denominator) < 1e-10) {
                throw new ArithmeticException(
                        "Division by zero complex number"
                );
            }

            return new Complex(
                    (real * other.real
                            + imaginary * other.imaginary) / denominator,
                    (imaginary * other.real
                            - real * other.imaginary) / denominator
            );
        }

        double abs() {
            return Math.hypot(real, imaginary);
        }

        public String toString() {
            if (Math.abs(imaginary) < 1e-10) {
                return String.format("%.2f", real);
            }

            if (Math.abs(real) < 1e-10) {
                return String.format("%.2fi", imaginary);
            }

            String sign = imaginary >= 0 ? " + " : " - ";

            return String.format("%.2f", real)
                    + sign
                    + String.format("%.2f", Math.abs(imaginary))
                    + "i";
        }
    }

    // Matrix of complex numbers
    static class ComplexMatrix {
        private final int rows;
        private final int cols;
        private final Complex[][] data;

        ComplexMatrix(int rows, int cols) {
            if (rows <= 0 || cols <= 0) {
                throw new IllegalArgumentException(
                        "Matrix dimensions must be positive"
                );
            }

            this.rows = rows;
            this.cols = cols;
            data = new Complex[rows][cols];

            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    data[i][j] = new Complex(0, 0);
                }
            }
        }

        int getRows() {
            return rows;
        }

        int getCols() {
            return cols;
        }

        Complex get(int row, int col) {
            return data[row][col];
        }

        void set(int row, int col, Complex value) {
            data[row][col] = value;
        }

        // Read matrix from console
        static ComplexMatrix readMatrix(String name) {
            System.out.println("\nEnter matrix " + name);

            int rows = readPositiveInt("Number of rows: ");
            int cols = readPositiveInt("Number of columns: ");

            ComplexMatrix matrix = new ComplexMatrix(rows, cols);

            System.out.println(
                    "Enter real and imaginary parts separately."
            );

            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    System.out.println(
                            "Element [" + (i + 1) + "][" + (j + 1) + "]:"
                    );

                    double real = readDouble("Real part: ");
                    double imaginary = readDouble("Imaginary part: ");

                    matrix.set(i, j, new Complex(real, imaginary));
                }
            }

            return matrix;
        }

        // Matrix addition
        ComplexMatrix add(ComplexMatrix other) {
            checkSameSize(other);

            ComplexMatrix result =
                    new ComplexMatrix(rows, cols);

            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    result.data[i][j] =
                            data[i][j].add(other.data[i][j]);
                }
            }

            return result;
        }

        // Matrix multiplication
        ComplexMatrix multiply(ComplexMatrix other) {
            if (cols != other.rows) {
                throw new IllegalArgumentException(
                        "Columns of the first matrix must equal "
                                + "rows of the second matrix"
                );
            }

            ComplexMatrix result =
                    new ComplexMatrix(rows, other.cols);

            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < other.cols; j++) {
                    Complex sum = new Complex(0, 0);

                    for (int k = 0; k < cols; k++) {
                        sum = sum.add(
                                data[i][k].multiply(other.data[k][j])
                        );
                    }

                    result.data[i][j] = sum;
                }
            }

            return result;
        }

        // Matrix transpose
        ComplexMatrix transpose() {
            ComplexMatrix result =
                    new ComplexMatrix(cols, rows);

            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    result.data[j][i] = data[i][j];
                }
            }

            return result;
        }

        // Determinant using Gaussian elimination
        Complex determinant() {
            if (rows != cols) {
                throw new IllegalArgumentException(
                        "Determinant requires a square matrix"
                );
            }

            Complex[][] copy = copyData();
            Complex det = new Complex(1, 0);

            for (int k = 0; k < rows; k++) {
                int pivot = k;

                for (int i = k + 1; i < rows; i++) {
                    if (copy[i][k].abs() > copy[pivot][k].abs()) {
                        pivot = i;
                    }
                }

                if (copy[pivot][k].abs() < 1e-10) {
                    return new Complex(0, 0);
                }

                if (pivot != k) {
                    Complex[] temp = copy[k];
                    copy[k] = copy[pivot];
                    copy[pivot] = temp;

                    det = det.multiply(new Complex(-1, 0));
                }

                Complex pivotValue = copy[k][k];
                det = det.multiply(pivotValue);

                for (int i = k + 1; i < rows; i++) {
                    Complex factor = copy[i][k].divide(pivotValue);

                    for (int j = k + 1; j < cols; j++) {
                        copy[i][j] = copy[i][j].subtract(
                                factor.multiply(copy[k][j])
                        );
                    }
                }
            }

            return det;
        }

        // Matrix inverse using Gauss-Jordan elimination
        ComplexMatrix inverse() {
            if (rows != cols) {
                throw new IllegalArgumentException(
                        "Inverse requires a square matrix"
                );
            }

            int n = rows;
            Complex[][] left = copyData();
            Complex[][] right = new Complex[n][n];

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    right[i][j] = new Complex(
                            i == j ? 1 : 0, 0
                    );
                }
            }

            for (int k = 0; k < n; k++) {
                int pivot = k;

                for (int i = k + 1; i < n; i++) {
                    if (left[i][k].abs() > left[pivot][k].abs()) {
                        pivot = i;
                    }
                }

                if (left[pivot][k].abs() < 1e-10) {
                    throw new ArithmeticException(
                            "Matrix is singular and has no inverse"
                    );
                }

                Complex[] temp = left[k];
                left[k] = left[pivot];
                left[pivot] = temp;

                temp = right[k];
                right[k] = right[pivot];
                right[pivot] = temp;

                Complex divisor = left[k][k];

                for (int j = 0; j < n; j++) {
                    left[k][j] = left[k][j].divide(divisor);
                    right[k][j] = right[k][j].divide(divisor);
                }

                for (int i = 0; i < n; i++) {
                    if (i == k) {
                        continue;
                    }

                    Complex factor = left[i][k];

                    for (int j = 0; j < n; j++) {
                        left[i][j] = left[i][j].subtract(
                                factor.multiply(left[k][j])
                        );

                        right[i][j] = right[i][j].subtract(
                                factor.multiply(right[k][j])
                        );
                    }
                }
            }

            ComplexMatrix result = new ComplexMatrix(n, n);

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    result.data[i][j] = right[i][j];
                }
            }

            return result;
        }

        // Matrix division: A / B = A * inverse(B)
        ComplexMatrix divide(ComplexMatrix other) {
            if (other.rows != other.cols) {
                throw new IllegalArgumentException(
                        "The second matrix must be square"
                );
            }

            return multiply(other.inverse());
        }

        private void checkSameSize(ComplexMatrix other) {
            if (rows != other.rows || cols != other.cols) {
                throw new IllegalArgumentException(
                        "Matrices must have the same dimensions"
                );
            }
        }

        private Complex[][] copyData() {
            Complex[][] copy = new Complex[rows][cols];

            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    copy[i][j] = new Complex(
                            data[i][j].real,
                            data[i][j].imaginary
                    );
                }
            }

            return copy;
        }

        // Print matrix
        public String toString() {
            StringBuilder result = new StringBuilder();

            for (int i = 0; i < rows; i++) {
                result.append("[ ");

                for (int j = 0; j < cols; j++) {
                    result.append(data[i][j]);

                    if (j < cols - 1) {
                        result.append(", ");
                    }
                }

                result.append(" ]\n");
            }

            return result.toString();
        }
    }

    // Safe console input
    static int readPositiveInt(String message) {
        while (true) {
            System.out.print(message);

            if (scanner.hasNextInt()) {
                int value = scanner.nextInt();

                if (value > 0) {
                    return value;
                }
            } else {
                scanner.next();
            }

            System.out.println("Enter a positive integer.");
        }
    }

    static int readMenuChoice() {
        System.out.print("Choose an operation: ");

        if (scanner.hasNextInt()) {
            return scanner.nextInt();
        }

        scanner.next();
        return -1;
    }

    static double readDouble(String message) {
        while (true) {
            System.out.print(message);

            if (scanner.hasNextDouble()) {
                return scanner.nextDouble();
            }

            scanner.next();
            System.out.println("Enter a valid number.");
        }
    }

    // Main console interface
    public static void main(String[] args) {
        while (true) {
            System.out.println("1. Add matrices");
            System.out.println("2. Multiply matrices");
            System.out.println("3. Divide matrices");
            System.out.println("4. Transpose a matrix");
            System.out.println("5. Calculate determinant");
            System.out.println("0. Exit");

            int choice = readMenuChoice();

            if (choice == 0) {
                System.out.println("Program finished.");
                break;
            }

            try {
                switch (choice) {
                    case 1: {
                        ComplexMatrix a =
                                ComplexMatrix.readMatrix("A");
                        ComplexMatrix b =
                                ComplexMatrix.readMatrix("B");

                        System.out.println("Matrix A:\n" + a);
                        System.out.println("Matrix B:\n" + b);
                        System.out.println("A + B:\n" + a.add(b));
                        break;
                    }

                    case 2: {
                        ComplexMatrix a =
                                ComplexMatrix.readMatrix("A");
                        ComplexMatrix b =
                                ComplexMatrix.readMatrix("B");

                        System.out.println("A * B:\n" + a.multiply(b));
                        break;
                    }

                    case 3: {
                        ComplexMatrix a =
                                ComplexMatrix.readMatrix("A");
                        ComplexMatrix b =
                                ComplexMatrix.readMatrix("B");

                        System.out.println("A / B:\n" + a.divide(b));
                        break;
                    }

                    case 4: {
                        ComplexMatrix a =
                                ComplexMatrix.readMatrix("A");

                        System.out.println(
                                "Transposed matrix:\n" + a.transpose()
                        );
                        break;
                    }

                    case 5: {
                        ComplexMatrix a =
                                ComplexMatrix.readMatrix("A");

                        System.out.println(
                                "Determinant: " + a.determinant()
                        );
                        break;
                    }

                    default:
                        System.out.println("Unknown operation.");
                }
            } catch (IllegalArgumentException
                     | ArithmeticException e) {
                System.out.println("Error: " + e.getMessage());
            }
        }

        scanner.close();
    }
}