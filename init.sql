
CREATE TABLE IF NOT EXISTS customers (
    customer_id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    nickname VARCHAR(255),
    contact_no VARCHAR(50),
    address TEXT,
    max_credit NUMERIC(10,2) DEFAULT 0,
    is_active INT DEFAULT 1
);

CREATE TABLE IF NOT EXISTS products (
    product_id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    category VARCHAR(255),
    unit_price NUMERIC(10,2) DEFAULT 0,
    stock_qty INT DEFAULT 0,
    allow_credit INT DEFAULT 1,
    is_active INT DEFAULT 1
);

CREATE TABLE IF NOT EXISTS credit_tx (
    tx_id SERIAL PRIMARY KEY,
    customer_id INT,
    product_id INT,
    tx_date DATE,
    tx_type VARCHAR(20),
    qty INT DEFAULT 0,
    unit_price NUMERIC(10,2) DEFAULT 0,
    amount NUMERIC(10,2) DEFAULT 0,
    notes TEXT
);
