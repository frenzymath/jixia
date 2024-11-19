import os
import json
import re
import datasets

def camel_to_snake(name):
    """Convert camelCase or PascalCase to snake_case."""
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

def convert_keys_to_snake_case(data):
    """Recursively convert all dictionary keys to snake_case."""
    if isinstance(data, dict):
        return {camel_to_snake(key): convert_keys_to_snake_case(value) for key, value in data.items()}
    elif isinstance(data, list):
        return [convert_keys_to_snake_case(item) for item in data]
    else:
        return data

def join_json_files(input_folder, output_file):
    combined_list = []

    for filename in os.listdir(input_folder):
        if filename.endswith(".json"):
            file_path = os.path.join(input_folder, filename)
            with open(file_path, "r", encoding="utf-8") as f:
                try:
                    data = json.load(f)
                    if isinstance(data, list):
                        # Convert keys to snake_case and add "path" key
                        base_filename = os.path.splitext(filename)[0]
                        for item in data:
                            if isinstance(item, dict):
                                item = convert_keys_to_snake_case(item)
                                item["path"] = base_filename
                            else:
                                print(f"Skipping non-dict item in file {filename}: {item}")
                            combined_list.append(item)
                    else:
                        print(f"Skipping file {filename} as it doesn't contain a list")
                except json.JSONDecodeError as e:
                    print(f"Error reading {filename}: {e}")


# Example usage
input_folder = "scripts/jixia-data-aug-v4.13.0"  # Replace with the folder containing your JSON files
output_file = "scripts/jixia-data-aug-v4.13.0-combined.json"  # Replace with your desired output file path
join_json_files(input_folder, output_file)

# Load the combined JSON file into a dataset
dataset = datasets.load_dataset("json", data_files=output_file)
dataset.push_to_hub("pkuAI4M/data-aug-test")
