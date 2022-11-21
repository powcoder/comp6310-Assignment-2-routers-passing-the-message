https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
package routers

import "log"

func Router(self RouterId, incoming <-chan interface{}, neighbours []chan<- interface{}, framework chan<- Envelope) {
	for {
		select {
		case raw := <-incoming:
			switch msg := raw.(type) {
			case Envelope:
				if msg.Dest == self {
					framework <- msg
				} else {
					// Handle forwarding on a message here
					msg.Hops += 1
					//neighbours[???] <- msg
				}
			// Add more cases to handle any other message types you create here
			default:
				log.Printf("[%v] received unexpected message %g\n", self, msg)
			}
		}
	}
}
