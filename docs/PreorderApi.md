# openapi_client.PreorderApi

All URIs are relative to *http://localhost/api/v2.0*

Method | HTTP request | Description
------------- | ------------- | -------------
[**preorder_account_id_get**](PreorderApi.md#preorder_account_id_get) | **GET** /preorder/{account_id} | Estimate trade margin for one order
[**preorder_account_id_post**](PreorderApi.md#preorder_account_id_post) | **POST** /preorder/{account_id} | Estimate trade margin for list of orders


# **preorder_account_id_get**
> PreorderCheckResponse preorder_account_id_get(account_id, symbol_id, quantity, currency=currency, price=price, show_margin_structure=show_margin_structure)

Estimate trade margin for one order

### Example


```python
import openapi_client
from openapi_client.models.preorder_check_response import PreorderCheckResponse
from openapi_client.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost/api/v2.0
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "http://localhost/api/v2.0"
)


# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = openapi_client.PreorderApi(api_client)
    account_id = 'FBB1234.001' # str | Account Id (default to 'FBB1234.001')
    symbol_id = 'EUR/USD.EXANTE' # str | 
    quantity = 35.68 # float | 
    currency = 'EUR' # str | Currency code (optional) (default to 'EUR')
    price = 99.71 # float |  (optional)
    show_margin_structure = False # bool |  (optional) (default to False)

    try:
        # Estimate trade margin for one order
        api_response = api_instance.preorder_account_id_get(account_id, symbol_id, quantity, currency=currency, price=price, show_margin_structure=show_margin_structure)
        print("The response of PreorderApi->preorder_account_id_get:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling PreorderApi->preorder_account_id_get: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **account_id** | **str**| Account Id | [default to &#39;FBB1234.001&#39;]
 **symbol_id** | **str**|  | 
 **quantity** | **float**|  | 
 **currency** | **str**| Currency code | [optional] [default to &#39;EUR&#39;]
 **price** | **float**|  | [optional] 
 **show_margin_structure** | **bool**|  | [optional] [default to False]

### Return type

[**PreorderCheckResponse**](PreorderCheckResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful margin check operation |  -  |
**400** | Invalid input parameters |  -  |
**404** | Account not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **preorder_account_id_post**
> PreorderCheckResponse preorder_account_id_post(account_id, preorder_account_id_post_request)

Estimate trade margin for list of orders

### Example


```python
import openapi_client
from openapi_client.models.preorder_account_id_post_request import PreorderAccountIdPostRequest
from openapi_client.models.preorder_check_response import PreorderCheckResponse
from openapi_client.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost/api/v2.0
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "http://localhost/api/v2.0"
)


# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = openapi_client.PreorderApi(api_client)
    account_id = 'FBB1234.001' # str | Account Id (default to 'FBB1234.001')
    preorder_account_id_post_request = openapi_client.PreorderAccountIdPostRequest() # PreorderAccountIdPostRequest | 

    try:
        # Estimate trade margin for list of orders
        api_response = api_instance.preorder_account_id_post(account_id, preorder_account_id_post_request)
        print("The response of PreorderApi->preorder_account_id_post:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling PreorderApi->preorder_account_id_post: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **account_id** | **str**| Account Id | [default to &#39;FBB1234.001&#39;]
 **preorder_account_id_post_request** | [**PreorderAccountIdPostRequest**](PreorderAccountIdPostRequest.md)|  | 

### Return type

[**PreorderCheckResponse**](PreorderCheckResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful margin check operation for multiple orders |  -  |
**400** | Invalid input parameters |  -  |
**404** | Account not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

