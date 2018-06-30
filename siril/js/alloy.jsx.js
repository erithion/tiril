// Generated from alloy.jsx
'use strict';

var React = AlloyEditor.React;

var ButtonMerge = React.createClass({
    displayName: 'ButtonMerge',

    mixins: [AlloyEditor.ButtonStyle, AlloyEditor.ButtonStateClasses],

    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'merge'
    },

    getDefaultProps: function getDefaultProps() {
        return {
            style: {
                element: 'merge'
            }
        };
    },

    render: function render() {
        var cssClass = 'ae-button ' + this.getStateClasses();

        return React.createElement(
            'button',
            { 'aria-pressed': cssClass.indexOf('pressed') !== -1,
                className: cssClass,
                onClick: this._merge,
                tabIndex: this.props.tabIndex,
                title: 'Merge paragraphs' },
            React.createElement('span', { className: 'ae-icon-align-justified' })
        );
    },

    _merge: function _merge(event) {
        var editor = this.props.editor.get('nativeEditor');
        var text = editor.getSelection().getSelectedText();
        editor.insertText(text.replace(/\s+/g, ' '));
    }
});
AlloyEditor.Buttons[ButtonMerge.key] = AlloyEditor.ButtonMerge = ButtonMerge;

var ButtonChapter = React.createClass({
    displayName: 'ButtonChapter',

    mixins: [AlloyEditor.ButtonStyle, AlloyEditor.ButtonStateClasses, AlloyEditor.ButtonActionStyle],

    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'chapter'
    },

    getDefaultProps: function getDefaultProps() {
        return {
            style: {
                element: 'h1'
            }
        };
    },

    render: function render() {
        var cssClass = 'ae-button ' + this.getStateClasses();

        return React.createElement(
            'button',
            { 'aria-pressed': cssClass.indexOf('pressed') !== -1,
                className: cssClass,
                onClick: this.applyStyle,
                tabIndex: this.props.tabIndex,
                title: 'Mark a chapter' },
            React.createElement('span', { className: 'ae-icon-h1' })
        );
    }
});
AlloyEditor.Buttons[ButtonChapter.key] = AlloyEditor.ButtonChapter = ButtonChapter;

var ButtonName = React.createClass({
    displayName: 'ButtonName',

    mixins: [AlloyEditor.ButtonStyle, AlloyEditor.ButtonStateClasses, AlloyEditor.ButtonActionStyle],

    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'name'
    },

    getDefaultProps: function getDefaultProps() {
        return {
            style: {
                element: 'h2'
            }
        };
    },

    render: function render() {
        var cssClass = 'ae-button ' + this.getStateClasses();

        return React.createElement(
            'button',
            { 'aria-pressed': cssClass.indexOf('pressed') !== -1,
                className: cssClass,
                onClick: this.applyStyle,
                tabIndex: this.props.tabIndex,
                title: 'Mark the chapter\'s name' },
            React.createElement('span', { className: 'ae-icon-h2' })
        );
    }
});
AlloyEditor.Buttons[ButtonName.key] = AlloyEditor.ButtonName = ButtonName;

var ButtonFile = React.createClass({
    displayName: 'ButtonFile',

    propTypes: {
        editor: React.PropTypes.object.isRequired
    },

    statics: {
        key: 'txt'
    },

    render: function render() {
        var inputSyle = { display: 'none' };

        return React.createElement(
            'div',
            null,
            React.createElement(
                'button',
                { className: 'ae-button',
                    onClick: this.handleClick,
                    tabIndex: this.props.tabIndex,
                    title: 'Load a text-file here' },
                React.createElement('span', { className: 'ae-icon-add' })
            ),
            React.createElement('input', { accept: '.txt', onChange: this._onInputChange, ref: 'fileInput', style: inputSyle, type: 'file' })
        );
    },

    handleClick: function handleClick(event) {
        AlloyEditor.ReactDOM.findDOMNode(this.refs.fileInput).click();
    },

    _onInputChange: function _onInputChange() {
        var inputEl = AlloyEditor.ReactDOM.findDOMNode(this.refs.fileInput);

        if (!inputEl.files.length) {
            return;
        }

        var reader = new FileReader();
        var file = inputEl.files[0];

        reader.onload = function (event) {
            var editor = this.props.editor.get('nativeEditor');
            var re = event.target.result.replace(/(.+)/g, '<p>$1</p>').replace(/[\n\r]/g, '');
            editor.insertHtml(re);
        }.bind(this);

        reader.readAsText(file);

        inputEl.value = '';
    }
});
AlloyEditor.Buttons[ButtonFile.key] = AlloyEditor.ButtonFile = ButtonFile;