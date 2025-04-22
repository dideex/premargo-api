# PreorderCheckResponse


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**margin** | **float** | Required margin for the orders | [optional] 
**free_money** | **float** | Available free money | [optional] 
**total_free_money** | **float** | Total available free money | [optional] 
**delta** | **float** | Margin change | [optional] 
**commission_charge_account_id** | **str** | Account ID for commission charges | [optional] 
**expected_commission** | **float** | Expected commission for the orders | [optional] 
**new_orders_commission** | **float** | Commission for new orders | [optional] 
**margin_check_passed** | **bool** | Whether the margin check passed | [optional] 
**margin_structure** | [**MarginStructure**](MarginStructure.md) |  | [optional] 
**warnings** | **List[str]** | List of warnings if any | [optional] 

## Example

```python
from openapi_client.models.preorder_check_response import PreorderCheckResponse

# TODO update the JSON string below
json = "{}"
# create an instance of PreorderCheckResponse from a JSON string
preorder_check_response_instance = PreorderCheckResponse.from_json(json)
# print the JSON string representation of the object
print(PreorderCheckResponse.to_json())

# convert the object into a dict
preorder_check_response_dict = preorder_check_response_instance.to_dict()
# create an instance of PreorderCheckResponse from a dict
preorder_check_response_from_dict = PreorderCheckResponse.from_dict(preorder_check_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


