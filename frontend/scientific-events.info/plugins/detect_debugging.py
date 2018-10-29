#coding:utf-8

def preBuildPage(site, page, context, data):
    """
    Check that the page is being generated as a part of 'cactus serve' command.
    If so, add DEBUG variable to the context. This variable may be checked in
    templates. 
    """
    if hasattr(site, '_port') and site._port is not None:
        context['DEBUG'] = True

    return context, data
