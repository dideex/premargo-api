# PreorderAccountIdPostRequest


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**currency** | **str** | Currency code | [optional] [default to 'EUR']
**orders_data** | [**List[PreorderAccountIdPostRequestOrdersDataInner]**](PreorderAccountIdPostRequestOrdersDataInner.md) |  | 
**show_margin_structure** | **bool** |  | [optional] [default to False]

## Example

```python
from openapi_client.models.preorder_account_id_post_request import PreorderAccountIdPostRequest

# TODO update the JSON string below
json = "{}"
# create an instance of PreorderAccountIdPostRequest from a JSON string
preorder_account_id_post_request_instance = PreorderAccountIdPostRequest.from_json(json)
# print the JSON string representation of the object
print(PreorderAccountIdPostRequest.to_json())

# convert the object into a dict
preorder_account_id_post_request_dict = preorder_account_id_post_request_instance.to_dict()
# create an instance of PreorderAccountIdPostRequest from a dict
preorder_account_id_post_request_from_dict = PreorderAccountIdPostRequest.from_dict(preorder_account_id_post_request_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


