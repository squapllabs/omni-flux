export function CaseInsensitiveFilter(
    allData: any[],
    globalSearch: string,
    propertiesToFilter: string[]
): any[] {
    return allData.filter((iterator) => {
        const lowerGlobalSearch = globalSearch.toLowerCase();

        for (const property of propertiesToFilter) {
            const value = getProperty(iterator, property);
            if (value && value.toLowerCase().includes(lowerGlobalSearch)) {
                return true; // Return true if any property matches the search
            }
        }

        return false; // Return false if no properties match the search
    });
}

function getProperty(obj: any, path: string) {
    return path.split('.').reduce((obj, path) => (obj ? obj[path] : undefined), obj);
}