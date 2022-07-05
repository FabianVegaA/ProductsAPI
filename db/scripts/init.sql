CREATE TABLE IF NOT EXISTS products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL UNIQUE,
  price INTEGER NOT NULL,
  description TEXT
);

INSERT INTO products (name, price, description) VALUES 
('Product 1', 10, 'Description 1'),
('Product 2', 20, 'Description 2'),
('Product 3', 30, 'Description 3');