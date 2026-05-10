def main():
    import pandas as pd
    from urllib.parse import unquote
    import io
    from selenium import webdriver
    from selenium.webdriver.common.by import By
    import time
    import os
    import re
    import undetected_chromedriver as uc

    # Add the country names here. It creates a folder inside to the base_save_path 
    countries = [
        "germany"
    ]

    #Add topic names. must be same as page url
    topics = [
        "energy-mix", "emissions"#, "electricity", "efficiency-demand", 
        #"renewables", "oil", "natural gas", "coal"
    ]

    base_save_path = r"/home/vaibhavagarwal/Projects/Scraping_Archieve/Teaching/Policy_LM/data_test"

    options = uc.ChromeOptions()
    # options.add_argument('--headless') 
    # driver = uc.Chrome(options=options)
    driver = uc.Chrome(options=options, version_main=147)

    for country in countries:
            # Create a specific folder for each country
            country_folder = os.path.join(base_save_path, country.replace("-", "_").title())
            if not os.path.exists(country_folder):
                os.makedirs(country_folder)
                
            for topic in topics:
                topic_url = topic.replace(" ", "-") 
                url = f"https://www.iea.org/countries/{country}/{topic_url}"
            
                driver.get(url)
                
                #content loads later so scroll down 20 times. If page is longer add more, but this seemed to work. no page is very long but all work with 20. 
                for i in range(20):
                    driver.execute_script(f"window.scrollTo(0, {i * 800});")
                    time.sleep(1.5) 

                elements = driver.find_elements(By.CSS_SELECTOR, ".a-link.a-link--sixary")

                for el in elements:
                    href = el.get_attribute("href")
                    
                    # to get the data which is stored in a link xD
                    if href and href.startswith("data:text/csv"):
                        raw_data = href.split(",", 1)[1]
                        decoded_csv = unquote(raw_data)
                        df = pd.read_csv(io.StringIO(decoded_csv))
                        
                        # Verify data exists and isn't a placeholder
                        if len(df) > 1 and "---" not in str(df.iloc[0, 0]):
                            # Get filename from 'download' attribute
                            original_filename = el.get_attribute("download")
                            clean_name = re.sub(r'[^\w\s\.-]', '', original_filename)
                            
                            #save 
                            full_file_path = os.path.join(country_folder, clean_name)
                            df.to_csv(full_file_path, index=False)
                            
                print(f"done-{topic}")

    driver.quit()


if __name__ == "__main__":
    main()