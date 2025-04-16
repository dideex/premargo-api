# coding: utf-8

"""
    OpenAPI Template

    There is a new description.

    The version of the OpenAPI document: 1.0.0
    Generated by OpenAPI Generator (https://openapi-generator.tech)

    Do not edit the class manually.
"""  # noqa: E501


import unittest

from openapi_client.models.pet import Pet

class TestPet(unittest.TestCase):
    """Pet unit test stubs"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def make_instance(self, include_optional) -> Pet:
        """Test Pet
            include_optional is a boolean, when False only required
            params are included, when True both required and
            optional params are included """
        # uncomment below to create an instance of `Pet`
        """
        model = Pet()
        if include_optional:
            return Pet(
                id = 56,
                category = openapi_client.models.pet_category.Pet category(
                    id = 56, 
                    name = 'CbUUGjjNSwg0_bs9ZayIMrKdgNvb6gvxmPb9GcsM61ate1RA89q3w1l4eH4XxEz.5awLMdeXylwK0lMGUSM4jsrh4dstlnQUN5vVdMLPA', ),
                name = 'doggie',
                photo_urls = [
                    ''
                    ],
                tags = [
                    openapi_client.models.pet_tag.Pet Tag(
                        id = 56, 
                        name = '', )
                    ],
                status = 'available'
            )
        else:
            return Pet(
                name = 'doggie',
                photo_urls = [
                    ''
                    ],
        )
        """

    def testPet(self):
        """Test Pet"""
        # inst_req_only = self.make_instance(include_optional=False)
        # inst_req_and_optional = self.make_instance(include_optional=True)

if __name__ == '__main__':
    unittest.main()
