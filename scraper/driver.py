
import pandas as pd

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options


def exception_handler(driver, mode, ele):
    # handles the exceptions when no element(s) is found
    # INPUT
    # :driver --> web driver
    # :mode --> based on tag name/ css name/ id/ xpath
    # :ele --> name of the element
    # OUTPUT: return true if the element does exist, else false
    try:

        def elements_by_name(driver, ele):
            driver.find_elements(By.NAME, ele)

        def elements_by_xpath(driver, ele):
            driver.find_elements(By.XPATH, ele)

        def elements_by_link_text(driver, ele):
            driver.find_elements(By.LINK_TEXT, ele)

        def elements_by_partial_link_text(driver, ele):
            driver.find_elements(By.PARTIAL_LINK_TEXT, ele)

        def elements_by_tag_name(driver, ele):
            driver.find_elements(By.TAG_NAME, ele)

        def elements_by_class_name(driver, ele):
            driver.find_elements(By.CLASS_NAME, ele)

        def elements_by_css_selector(driver, ele):
            driver.find_elements(By.CSS_SELECTOR, ele)

        def element_by_id(driver, ele):
            driver.find_element(By.ID, ele)

        def element_by_name(driver, ele):
            driver.find_element(By.NAME, ele)

        def element_by_xpath(driver, ele):
            driver.find_element(By.XPATH, ele)

        def element_by_link_text(driver, ele):
            driver.find_element(By.LINK_TEXT, ele)

        def element_by_partial_link_text(driver, ele):
            driver.find_element(By.PARTIAL_LINK_TEXT, ele)

        def element_by_tag_name(driver, ele):
            driver.find_element(By.TAG_NAME, ele)

        def element_by_class_name(driver, ele):
            driver.find_element(By.CLASS_NAME, ele)

        def element_by_css_selector(driver, ele):
            driver.find_element(By.CSS_SELECTOR, ele)

        def elements_by_id(driver, ele):
            driver.find_elements(By.ID, ele)

        DEFAULT_MODE = {
            # multiple elements
            0: elements_by_name,
            1: elements_by_xpath,
            2: elements_by_link_text,
            3: elements_by_partial_link_text,
            4: elements_by_tag_name,
            5: elements_by_class_name,
            6: elements_by_css_selector,
            15: elements_by_id,
            # single element
            7: element_by_id,
            8: element_by_name,
            9: element_by_xpath,
            10: element_by_link_text,
            11: element_by_partial_link_text,
            12: element_by_tag_name,
            13: element_by_class_name,
            14: element_by_css_selector
        }

        DEFAULT_MODE[mode](driver, ele)

    except:
        return False

    return True


if __name__ == "__main__":

  ROOT_URL = "https://www.udemy.com/"

  options = Options()
  options.headless = True

  driver = webdriver.Firefox(options=options)

  driver.get(ROOT_URL)

  # u154-popper-trigger--1
  temps = driver.find_elements(By.ID, "u154-popper-trigger--1")

  categoryBtn = temps[0]
  



