# PreorderAccountIdPostRequestOrdersDataInner


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**price** | **float** |  | [optional] 
**quantity** | **float** |  | 
**symbol_id** | **str** |  | 

## Example

```python
from openapi_client.models.preorder_account_id_post_request_orders_data_inner import PreorderAccountIdPostRequestOrdersDataInner

# TODO update the JSON string below
json = "{}"
# create an instance of PreorderAccountIdPostRequestOrdersDataInner from a JSON string
preorder_account_id_post_request_orders_data_inner_instance = PreorderAccountIdPostRequestOrdersDataInner.from_json(json)
# print the JSON string representation of the object
print(PreorderAccountIdPostRequestOrdersDataInner.to_json())

# convert the object into a dict
preorder_account_id_post_request_orders_data_inner_dict = preorder_account_id_post_request_orders_data_inner_instance.to_dict()
# create an instance of PreorderAccountIdPostRequestOrdersDataInner from a dict
preorder_account_id_post_request_orders_data_inner_from_dict = PreorderAccountIdPostRequestOrdersDataInner.from_dict(preorder_account_id_post_request_orders_data_inner_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


