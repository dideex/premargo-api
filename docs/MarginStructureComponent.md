# MarginStructureComponent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**asset** | **str** | Asset identifier | [optional] 
**asset_type** | **str** |  | [optional] 
**position** | **float** |  | [optional] 
**price** | **float** |  | [optional] 
**currency** | **str** |  | [optional] 
**crossrate** | **float** |  | [optional] 
**leverage_rate** | **float** |  | [optional] 
**margin** | **float** |  | [optional] 
**conv_margin** | **float** |  | [optional] 
**extreme_margin** | **float** |  | [optional] 
**conv_extreme_margin** | **float** |  | [optional] 
**effective_qtty** | **float** |  | [optional] 

## Example

```python
from openapi_client.models.margin_structure_component import MarginStructureComponent

# TODO update the JSON string below
json = "{}"
# create an instance of MarginStructureComponent from a JSON string
margin_structure_component_instance = MarginStructureComponent.from_json(json)
# print the JSON string representation of the object
print(MarginStructureComponent.to_json())

# convert the object into a dict
margin_structure_component_dict = margin_structure_component_instance.to_dict()
# create an instance of MarginStructureComponent from a dict
margin_structure_component_from_dict = MarginStructureComponent.from_dict(margin_structure_component_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


