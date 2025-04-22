# MarginStructure


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**before** | [**List[MarginStructureComponent]**](MarginStructureComponent.md) |  | [optional] 
**after** | [**List[MarginStructureComponent]**](MarginStructureComponent.md) |  | [optional] 

## Example

```python
from openapi_client.models.margin_structure import MarginStructure

# TODO update the JSON string below
json = "{}"
# create an instance of MarginStructure from a JSON string
margin_structure_instance = MarginStructure.from_json(json)
# print the JSON string representation of the object
print(MarginStructure.to_json())

# convert the object into a dict
margin_structure_dict = margin_structure_instance.to_dict()
# create an instance of MarginStructure from a dict
margin_structure_from_dict = MarginStructure.from_dict(margin_structure_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


