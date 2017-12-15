# imports - third-party modules
from scrapy.spiders        import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from scrapy.http           import FormRequest
from bs4                   import BeautifulSoup

# imports - module imports
from cache import write

class HistDataSpider(CrawlSpider):
    name             = 'histdata'
    allowed_domains  = ['histdata.com']
    start_urls       = ['http://histdata.com/download-free-forex-data']

    rules            = [
        Rule(
            LinkExtractor(
                allow = 'download-free-forex-historical-data/\?/[a-z]+/(-?[a-z]+)+/[a-z]{6}/[0-9]{4}/[0-9]{1,2}'
            )
        , callback    = 'parse_item'),
        Rule(
            LinkExtractor(
                allow = 'download-free-forex(-historical)?-data/\?/([a-z]+/(-?[a-z]+)+(/([a-z]{6}|[A-Z]{6})?(/[0-9]{4})?)?)?'
            )
        , follow      = True)
    ]

    def parse_item(self, response):
        body  = response.body
        soup  = BeautifulSoup(body, 'html.parser')
        form  = soup.find(id = 'file_down')
        data  = dict((dom.get('name'), dom.get('value'))
            for dom in form.find_all('input')
        )

        req   = FormRequest.from_response(
            response = response,
            formid   = 'file_down',
            formdata = data,
            callback = lambda response: write(response, HistDataSpider.name)
        )

        return req
