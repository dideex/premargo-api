# PreorderCheckPostRequest


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**currency** | **str** | Currency code | [optional] [default to 'EUR']
**orders_data** | [**List[PreorderCheckPostRequestOrdersDataInner]**](PreorderCheckPostRequestOrdersDataInner.md) |  | 
**show_margin_structure** | **bool** |  | [optional] [default to False]

## Example

```python
from openapi_client.models.preorder_check_post_request import PreorderCheckPostRequest

# TODO update the JSON string below
json = "{}"
# create an instance of PreorderCheckPostRequest from a JSON string
preorder_check_post_request_instance = PreorderCheckPostRequest.from_json(json)
# print the JSON string representation of the object
print(PreorderCheckPostRequest.to_json())

# convert the object into a dict
preorder_check_post_request_dict = preorder_check_post_request_instance.to_dict()
# create an instance of PreorderCheckPostRequest from a dict
preorder_check_post_request_from_dict = PreorderCheckPostRequest.from_dict(preorder_check_post_request_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


