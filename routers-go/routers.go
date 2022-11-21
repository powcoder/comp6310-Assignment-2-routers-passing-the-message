https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
package routers

type RouterId uint

type Template [][]RouterId

type Envelope struct {
	Dest    RouterId
	Hops    uint
	Message interface{}
}

type TestMessage int

func MakeRouters(t Template) (in []chan<- interface{}, out <-chan Envelope) {
	channels := make([]chan interface{}, len(t))
	framework := make(chan Envelope)

	in = make([]chan<- interface{}, len(t))
	for i := range channels {
		channels[i] = make(chan interface{})
		in[i] = channels[i]
	}
	out = framework

	for routerId, neighbourIds := range t {
		neighbours := make([]chan<- interface{}, len(neighbourIds))
		for i, id := range neighbourIds {
			neighbours[i] = channels[id]
		}

		go Router(RouterId(routerId), channels[routerId], neighbours, framework)
	}

	return
}
