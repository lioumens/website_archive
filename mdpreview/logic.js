var MarkdownBox = React.createClass({
    getInitialState: function() {
        return {text: '# Example Document\n Try entering some *markdown*!'};
    },
    handleTextChange: function(text) {
        this.setState({text:text});
    },
    render: function() {
        return (<div className="markdownBox">
                <h1 className="title">Markdown Previewer</h1>
                <h4 className="title">Made with <a href="https://facebook.github.io/react/">React.js</a> and <a href="https://github.com/chjj/marked">marked</a></h4>
                <MarkdownForm 
                    mdtext={this.state.text} 
                    onTextChange={this.handleTextChange}
                    />
                <MarkdownOutput 
                mdtext={this.state.text}/>
                </div>
               );
    }
});

var MarkdownForm = React.createClass({
    handleChange: function() {
        this.props.onTextChange(this.refs.formText.value);
    },
    render: function() {
        return (
                <div className="markdownForm">
                <h3>Input:</h3>
                <textarea 
                cols="80" 
                rows="80"
                value={this.props.mdtext}
                ref="formText"
                onChange={this.handleChange}
                ></textarea>
                </div>
               );
    }
});

var MarkdownOutput = React.createClass({
    rawMarkup: function () {
        var rawMarkup = marked(this.props.mdtext.toString(), {sanitize: true});
        return {__html: rawMarkup};
    },
    render: function() {
        return (<div className="markdownOutput">
                <h3>Output:</h3>
                <div className="textarea">
                <span dangerouslySetInnerHTML={this.rawMarkup()} />
                </div>
                </div>
               );
    }
});


ReactDOM.render(
        <MarkdownBox />,
        document.getElementById('content')
        );
