# PreorderCheckPostRequestOrdersDataInner


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**price** | **float** |  | [optional] 
**quantity** | **float** |  | 
**symbol_id** | **str** |  | 

## Example

```python
from openapi_client.models.preorder_check_post_request_orders_data_inner import PreorderCheckPostRequestOrdersDataInner

# TODO update the JSON string below
json = "{}"
# create an instance of PreorderCheckPostRequestOrdersDataInner from a JSON string
preorder_check_post_request_orders_data_inner_instance = PreorderCheckPostRequestOrdersDataInner.from_json(json)
# print the JSON string representation of the object
print(PreorderCheckPostRequestOrdersDataInner.to_json())

# convert the object into a dict
preorder_check_post_request_orders_data_inner_dict = preorder_check_post_request_orders_data_inner_instance.to_dict()
# create an instance of PreorderCheckPostRequestOrdersDataInner from a dict
preorder_check_post_request_orders_data_inner_from_dict = PreorderCheckPostRequestOrdersDataInner.from_dict(preorder_check_post_request_orders_data_inner_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


