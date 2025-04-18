# coding: utf-8

"""
    OpenAPI Template

    There is a new description.

    The version of the OpenAPI document: 1.0.4
    Generated by OpenAPI Generator (https://openapi-generator.tech)

    Do not edit the class manually.
"""  # noqa: E501


import unittest

from openapi_client.api.pet_api import PetApi


class TestPetApi(unittest.TestCase):
    """PetApi unit test stubs"""

    def setUp(self) -> None:
        self.api = PetApi()

    def tearDown(self) -> None:
        pass

    def test_add_pet(self) -> None:
        """Test case for add_pet

        Add a new pet to the store
        """
        pass

    def test_delete_pet(self) -> None:
        """Test case for delete_pet

        Deletes a pet
        """
        pass

    def test_find_pets_by_status(self) -> None:
        """Test case for find_pets_by_status

        Finds Pets by status
        """
        pass

    def test_find_pets_by_tags(self) -> None:
        """Test case for find_pets_by_tags

        Finds Pets by tags
        """
        pass

    def test_get_pet_by_id(self) -> None:
        """Test case for get_pet_by_id

        Find pet by ID
        """
        pass

    def test_update_pet(self) -> None:
        """Test case for update_pet

        Update an existing pet
        """
        pass

    def test_update_pet_with_form(self) -> None:
        """Test case for update_pet_with_form

        Updates a pet in the store with form data
        """
        pass

    def test_upload_file(self) -> None:
        """Test case for upload_file

        uploads an image
        """
        pass


if __name__ == '__main__':
    unittest.main()
