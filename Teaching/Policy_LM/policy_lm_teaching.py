import undetected_chromedriver as uc
from selenium.webdriver.common.by import By
import time

def main():
    driver = uc.Chrome(version_main=147) 
    
    url = "https://www.iea.org/countries/germany/energy-mix"
    driver.get(url)
    time.sleep(5)

    # Scroll down for data loading
    # driver.execute_script("window.scrollTo(0, 1000);")
    # time.sleep(2)

    # elements = driver.find_elements(By.CSS_SELECTOR, ".a-box-title")
   
    # for i in elements:
    #     link_text = i.text
    #     print(link_text)

    driver.quit()

if __name__ == "__main__":
    main()

