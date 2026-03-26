import ast

def dump_node(node, indent=0):
    """Recursively print AST nodes with indentation."""
    print('  ' * indent + type(node).__name__)
    for field, value in ast.iter_fields(node):
        if isinstance(value, list):
            for item in value:
                if isinstance(item, ast.AST):
                    dump_node(item, indent + 1)
        elif isinstance(value, ast.AST):
            dump_node(value, indent + 1)

if __name__ == "__main__":
    code = """
x = 5
y = x + 2
print(y)
"""
    tree = ast.parse(code)
    dump_node(tree)