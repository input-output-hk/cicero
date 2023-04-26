package repository

import "encoding/json"

type Page struct {
	Limit  int
	Offset int
	Total  int
}

func (self Page) Number() int {
	number := 1
	for ; number*self.Limit <= self.Offset; number += 1 {
	}
	return number
}

func (self Page) Pages() int {
	pages := self.Total / self.Limit
	if self.Total%self.Limit != 0 {
		pages += 1
	}
	return pages
}

func (self Page) PrevOffset() *int {
	offset := self.Offset - self.Limit
	if offset < 0 {
		offset = 0
	}
	if offset == self.Offset {
		return nil
	}
	return &offset
}

func (self Page) NextOffset() *int {
	offset := self.Offset + self.Limit
	if offset >= self.Total {
		return nil
	}
	return &offset
}

func (self *Page) MarshalJSON() ([]byte, error) {
	if self == nil {
		return []byte("null"), nil
	}
	return json.Marshal(map[string]int{
		"offset": self.Offset,
		"limit":  self.Limit,
	})
}
