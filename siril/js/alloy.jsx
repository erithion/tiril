// Use https://babeljs.io/repl/ to compile it to a regular JS
var React = AlloyEditor.React;

var ButtonMerge = React.createClass({
    mixins: [AlloyEditor.ButtonStyle, AlloyEditor.ButtonStateClasses],

    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'merge'
    },

    getDefaultProps: function() {
        return {
            style: {
                element: 'merge'
            }
        };
    },

    render: function() {
        var cssClass = 'ae-button ' + this.getStateClasses();

        return (
            <button aria-pressed={cssClass.indexOf('pressed') !== -1} 
                    className={cssClass} 
                    onClick={this._merge}
                    tabIndex={this.props.tabIndex} 
                    title="Merge paragraphs" >
                <span className="ae-icon-align-justified"></span>
            </button>
        );
        
    },
    
    _merge: function(event){
        var editor = this.props.editor.get('nativeEditor');
        var text = editor.getSelection().getSelectedText();
        editor.insertText(text.replace(/\s+/g, ' '));
    }
});
AlloyEditor.Buttons[ButtonMerge.key] = AlloyEditor.ButtonMerge = ButtonMerge;

    
var ButtonChapter = React.createClass({
    mixins: [AlloyEditor.ButtonStyle, AlloyEditor.ButtonStateClasses, AlloyEditor.ButtonActionStyle],

    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'chapter'
    },

    getDefaultProps: function() {
        return {
            style: {
                element: 'h1'
            }
        };
    },

    render: function() {
        var cssClass = 'ae-button ' + this.getStateClasses();

        return (
            <button aria-pressed={cssClass.indexOf('pressed') !== -1} 
                    className={cssClass} 
                    onClick={this.applyStyle} 
                    tabIndex={this.props.tabIndex} 
                    title="Mark a chapter">
                <span className="ae-icon-h1"></span>
            </button>
        );
        
    }
});
AlloyEditor.Buttons[ButtonChapter.key] = AlloyEditor.ButtonChapter = ButtonChapter;
    
var ButtonName = React.createClass({
    mixins: [AlloyEditor.ButtonStyle, AlloyEditor.ButtonStateClasses, AlloyEditor.ButtonActionStyle],

    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'name'
    },

    getDefaultProps: function() {
        return {
            style: {
                element: 'h2'
            }
        };
    },

    render: function() {
        var cssClass = 'ae-button ' + this.getStateClasses();

        return (
            <button aria-pressed={cssClass.indexOf('pressed') !== -1} 
                    className={cssClass} 
                    onClick={this.applyStyle} 
                    tabIndex={this.props.tabIndex} 
                    title="Mark the chapter's name">
                <span className="ae-icon-h2"></span>
            </button>
        );
        
    }
});
AlloyEditor.Buttons[ButtonName.key] = AlloyEditor.ButtonName = ButtonName;

var ButtonFile = React.createClass({
    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'txt'
    },

    render: function() {
        var inputSyle = {display: 'none'};

        return (
            <div>
                <button className="ae-button" 
                        onClick={this.handleClick} 
                        tabIndex={this.props.tabIndex} 
                        title="Load a text-file here">
                    <span className="ae-icon-add"></span>
                </button>

                <input accept=".txt" onChange={this._onInputChange} ref="fileInput" style={inputSyle} type="file"/>
            </div>
        );
    },

    handleClick: function(event) {
        AlloyEditor.ReactDOM.findDOMNode(this.refs.fileInput).click();
    },

    _onInputChange: function() {
        var inputEl = AlloyEditor.ReactDOM.findDOMNode(this.refs.fileInput);

        if (!inputEl.files.length) {
            return;
        }

        var reader = new FileReader();
        var file = inputEl.files[0];

        reader.onload = function(event) {
            var editor = this.props.editor.get('nativeEditor');
            var re = event.target.result.replace(/(.+)/g, '<p>$1</p>').replace(/[\n\r]/g, '');
            editor.insertHtml(re);
        }.bind(this);

        reader.readAsText(file);

        inputEl.value = '';
    }
});
AlloyEditor.Buttons[ButtonFile.key] = AlloyEditor.ButtonFile = ButtonFile;
