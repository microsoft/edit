-- Single-line comment

/*
 * Multi-line comment.
 * The comment should remain highlighted across lines.
 */

/* Outer comment
    /* Nested comment */ SELECT 'still a comment';
    /* Nested across
        lines */ still the outer comment
*/ SELECT 1;

-- String literals
SELECT 'hello', 'It''s a string';
SELECT N'national string', n'It''s also a string', '', '''';
SELECT 'an escaped quote at the end of this line: ''
still in the string', 42;

-- Quoted identifiers
SELECT "user", "first""name" FROM "users";
SELECT "escaped quote at end of line: ""
still an identifier", [ORDER], [name]]with]]brackets];

-- Backtick-quoted identifiers
SELECT `user`, `first``name` FROM `users`;
SELECT `escaped backtick at end of line: ``
still an identifier`, 42;

-- Numeric literals
SELECT 0, 42, -17, 3.14159, 1.5e-3, -1.5e-3, .5, -.5, 1., 1.e+3, 0xDEADBEEF;

select order_id, ascending, inner_value, integer_value, _123, @select, #table
from users order by order_id asc;
SELECT times, timestamps, sysdate, TRUE, FALSE, NULL;

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

CREATE TABLE type_prefixes (
    short_value INT,
    long_value INTEGER,
    duration INTERVAL,
    short_name CHAR(10),
    long_name CHARACTER(10),
    label VARCHAR(100),
    unicode_label NVARCHAR(100),
    amount DECIMAL(10, 2),
    day_value DATE,
    instant DATETIME,
    precise_instant DATETIME2,
    zoned_instant DATETIMEOFFSET,
    time_value TIME,
    stamp TIMESTAMP,
    zoned_stamp TIMESTAMPTZ
);

SELECT DISTINCT u.id AS user_id, u.name, u.age + 1 AS next_age, o.total
FROM users AS u
INNER JOIN orders AS o ON o.user_id = u.id
LEFT JOIN payments AS p ON p.order_id = o.id
WHERE u.age >= 18 AND u.age <= 65 AND o.total > 100
    AND u.name <> 'Unknown' AND u.name IS NOT NULL
    AND (u.active = TRUE OR u.email IS NULL)
    AND u.name LIKE 'A%' AND u.name ILIKE 'a%'
    AND u.age BETWEEN 18 AND 30 AND u.name IN ('Alice', 'Bob')
ORDER BY u.name ASC, o.total DESC LIMIT 10 OFFSET 20;

SELECT department, COUNT(*) AS employee_count
FROM employees
GROUP BY department HAVING COUNT(*) > 5;

SELECT name, CASE WHEN age < 18 THEN 'minor' ELSE 'adult' END AS age_group
FROM users;

SELECT LOWER(name), custom_function(id), CAST(age AS INTEGER) FROM users;
SELECT ROW_NUMBER() OVER (ORDER BY id) FROM users;

INSERT INTO users (name, email)
VALUES ('Alice', 'alice@example.com'), ('Bob', 'bob@example.com');
UPDATE users SET name = 'Updated', active = FALSE WHERE id = 42 RETURNING id;
DELETE FROM users WHERE active = FALSE;

CREATE INDEX idx_users_email ON users (email);
CREATE VIEW active_users AS SELECT id, name FROM users WHERE active = TRUE;
ALTER TABLE users ADD COLUMN created_at TIMESTAMP;
DROP VIEW active_users;

-- Common transaction keywords
BEGIN;
UPDATE users SET active = TRUE WHERE id = 1;
COMMIT;
BEGIN;
DELETE FROM users WHERE id != 1;
ROLLBACK;

-- CTE
WITH active_users AS (
    SELECT id, name FROM users WHERE active = TRUE
)
SELECT id, name FROM active_users
UNION
SELECT id, name FROM archived_users;

-- EXISTS
SELECT * FROM users AS u
WHERE EXISTS (SELECT 1 FROM orders AS o WHERE o.user_id = u.id);
