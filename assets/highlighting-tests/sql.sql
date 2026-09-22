-- Single-line comment

/*
 * Multi-line comment.
 * The comment should remain highlighted across lines.
 */

-- Basic SELECT
SELECT
    id,
    name,
    email
FROM
    users
WHERE
    active = TRUE;

-- Aliases and expressions
SELECT
    u.id AS user_id,
    u.name AS user_name,
    u.age + 1 AS next_age
FROM
    users AS u
WHERE
    u.age >= 18
    AND u.name IS NOT NULL;

-- String literals
SELECT
    'hello',
    'It''s a string',
    'single quotes are escaped by doubling them';

-- Quoted identifiers
SELECT
    "user",
    "first""name"
FROM
    "users";

-- Backtick-quoted identifiers
SELECT
    `user`,
    `first``name`
FROM
    `users`;

-- Numeric literals
SELECT
    0,
    42,
    -17,
    3.14159,
    1.5e -3;

-- Boolean and NULL literals
SELECT
    TRUE,
    FALSE,
    NULL;

-- Comparison and logical operators
SELECT
    *
FROM
    users
WHERE
    age >= 18
    AND age <= 65
    AND status != 'inactive'
    AND name <> 'Unknown'
    AND (
        active = TRUE
        OR verified = FALSE
    );

-- LIKE, IN and BETWEEN
SELECT
    *
FROM
    users
WHERE
    name LIKE 'A%'
    AND age BETWEEN 18
    AND 30
    AND status IN ('active', 'pending');

-- INSERT
INSERT INTO
    users (name, email, age)
VALUES
    ('Alice', 'alice@example.com', 25);

-- Multiple-row INSERT
INSERT INTO
    users (name, email)
VALUES
    ('Bob', 'bob@example.com'),
    ('Carol', 'carol@example.com');

-- UPDATE
UPDATE
    users
SET
    name = 'Updated',
    active = FALSE
WHERE
    id = 42;

-- DELETE
DELETE FROM
    users
WHERE
    active = FALSE;

-- JOINs
SELECT
    u.name,
    o.id,
    o.total
FROM
    users AS u
    INNER JOIN orders AS o ON o.user_id = u.id
    LEFT JOIN payments AS p ON p.order_id = o.id
WHERE
    o.total > 100;

-- GROUP BY and aggregate functions
SELECT
    department,
    COUNT(*) AS employee_count,
    SUM(salary) AS total_salary,
    AVG(salary) AS average_salary,
    MIN(salary) AS minimum_salary,
    MAX(salary) AS maximum_salary
FROM
    employees
GROUP BY
    department
HAVING
    COUNT(*) > 5
ORDER BY
    average_salary DESC;

-- DISTINCT, LIMIT and OFFSET
SELECT
    DISTINCT name
FROM
    users
ORDER BY
    name ASC
LIMIT
    10 OFFSET 20;

-- CASE expression
SELECT
    name,
    CASE
        WHEN age < 18 THEN 'minor'
        WHEN age >= 18
        AND age < 65 THEN 'adult'
        ELSE 'senior'
    END AS age_group
FROM
    users;

-- Common functions
SELECT
    LOWER(name),
    UPPER(name),
    LENGTH(name),
    SUBSTRING(name, 1, 5),
    ROUND(balance),
    ABS(balance),
    COALESCE(nickname, name),
    NULLIF(status, '');

-- CAST and conversion
SELECT
    CAST(age AS INTEGER),
    CONVERT(name, VARCHAR(100))
FROM
    users;

-- Common DDL
CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE,
    age INTEGER DEFAULT 0,
    active BOOLEAN DEFAULT TRUE
);

-- Constraints
CREATE TABLE orders (
    id INTEGER PRIMARY KEY,
    user_id INTEGER NOT NULL,
    total DECIMAL(10, 2) CHECK (total >= 0),
    CONSTRAINT fk_user FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Index
CREATE INDEX idx_users_email ON users (email);

-- View
CREATE VIEW active_users AS
SELECT
    id,
    name,
    email
FROM
    users
WHERE
    active = TRUE;

-- ALTER and DROP
ALTER TABLE
    users
ADD
    COLUMN created_at TIMESTAMP;

DROP VIEW active_users;

-- Common transaction keywords
BEGIN;

UPDATE
    users
SET
    active = TRUE
WHERE
    id = 1;

COMMIT;

BEGIN;

DELETE FROM
    users
WHERE
    id = 999;

ROLLBACK;

-- CTE
WITH active_users AS (
    SELECT
        id,
        name
    FROM
        users
    WHERE
        active = TRUE
)
SELECT
    *
FROM
    active_users
ORDER BY
    name;

-- UNION
SELECT
    id,
    name
FROM
    users
UNION
SELECT
    id,
    name
FROM
    archived_users;

-- EXISTS
SELECT
    *
FROM
    users AS u
WHERE
    EXISTS (
        SELECT
            1
        FROM
            orders AS o
        WHERE
            o.user_id = u.id
    );

-- NULL checks
SELECT
    *
FROM
    users
WHERE
    email IS NULL
    OR email IS NOT NULL;

-- Function calls with nested expressions
SELECT
    COALESCE(LOWER(name), 'unknown'),
    ROUND(ABS(balance), 2)
FROM
    accounts;
