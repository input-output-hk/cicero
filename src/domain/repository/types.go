package repository

type Page struct {
	Limit  int
	Offset int
	Total int
}

func (p *Page) Number() int {
	number := 1
	for ; number * p.Limit <= p.Offset; number += 1 {}
	return number
}

func (p *Page) Pages() int {
	pages := p.Total / p.Limit;
	if p.Total % p.Limit != 0 {
		pages += 1
	}
	return pages
}

func (p *Page) PrevOffset() *int {
	offset := p.Offset - p.Limit
	if offset < 0 {
		offset = 0
	}
	if offset == p.Offset {
		return nil
	}
	return &offset
}

func(p *Page) NextOffset() *int {
	offset := p.Offset + p.Limit
	if offset >= p.Total - 1 {
		return nil
	}
	return &offset
}
