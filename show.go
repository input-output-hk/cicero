package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type ShowCmd struct {
	ID     uint64 `arg:"--id" default:0`
	Inputs string `arg:"--inputs" default:"{}"`
}

func runShow(args *ShowCmd) error {
	return show(args)
}

func show(args *ShowCmd) error {
	def, err := nixInstantiate(args.ID, args.Inputs)
	if err != nil {
		return err
	}

	prettyJSON, err := json.MarshalIndent(def, "", "    ")
	if err != nil {
		log.Fatal("Failed to generate json", err)
	}
	fmt.Printf("%s\n", string(prettyJSON))

	return nil
}
