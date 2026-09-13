// SPDX-License-Identifier: AGPL-3.0-or-later

import {decodeHTMLEntities, htmlToMarkdown, stripHtmlTags} from '@app/api/utils/DOMUtils';
import {describe, expect, it} from 'vitest';

describe('decodeHTMLEntities', () => {
	it('returns empty string for null input', () => {
		expect(decodeHTMLEntities(null)).toBe('');
	});
	it('returns empty string for undefined input', () => {
		expect(decodeHTMLEntities(undefined)).toBe('');
	});
	it('returns empty string for empty string input', () => {
		expect(decodeHTMLEntities('')).toBe('');
	});
	it('decodes basic HTML entities', () => {
		expect(decodeHTMLEntities('&amp;')).toBe('&');
		expect(decodeHTMLEntities('&lt;')).toBe('<');
		expect(decodeHTMLEntities('&gt;')).toBe('>');
		expect(decodeHTMLEntities('&quot;')).toBe('"');
		expect(decodeHTMLEntities('&#39;')).toBe("'");
	});
	it('decodes numeric HTML entities', () => {
		expect(decodeHTMLEntities('&#60;')).toBe('<');
		expect(decodeHTMLEntities('&#x3C;')).toBe('<');
	});
	it('decodes mixed content', () => {
		expect(decodeHTMLEntities('Hello &amp; World')).toBe('Hello & World');
		expect(decodeHTMLEntities('&lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;')).toBe(
			'<script>alert("xss")</script>',
		);
	});
	it('preserves plain text without entities', () => {
		expect(decodeHTMLEntities('Hello World')).toBe('Hello World');
	});
	it('decodes special characters', () => {
		expect(decodeHTMLEntities('&nbsp;')).toBe('\u00A0');
		expect(decodeHTMLEntities('&copy;')).toBe('\u00A9');
		expect(decodeHTMLEntities('&euro;')).toBe('\u20AC');
	});
});

describe('stripHtmlTags', () => {
	it('returns empty string for null input', () => {
		expect(stripHtmlTags(null)).toBe('');
	});
	it('returns empty string for undefined input', () => {
		expect(stripHtmlTags(undefined)).toBe('');
	});
	it('returns empty string for empty string input', () => {
		expect(stripHtmlTags('')).toBe('');
	});
	it('strips simple HTML tags', () => {
		expect(stripHtmlTags('<p>Hello</p>')).toBe('Hello');
		expect(stripHtmlTags('<div>World</div>')).toBe('World');
	});
	it('strips self-closing tags', () => {
		expect(stripHtmlTags('Hello<br/>World')).toBe('HelloWorld');
		expect(stripHtmlTags('Hello<br />World')).toBe('HelloWorld');
	});
	it('strips tags with attributes', () => {
		expect(stripHtmlTags('<a href="https://example.com">Link</a>')).toBe('Link');
		expect(stripHtmlTags('<img src="image.png" alt="Image"/>')).toBe('');
	});
	it('strips nested tags', () => {
		expect(stripHtmlTags('<div><p><span>Nested</span></p></div>')).toBe('Nested');
	});
	it('preserves plain text', () => {
		expect(stripHtmlTags('Hello World')).toBe('Hello World');
	});
	it('strips multiple tags', () => {
		expect(stripHtmlTags('<p>One</p><p>Two</p><p>Three</p>')).toBe('OneTwoThree');
	});
	it('handles malformed tags', () => {
		expect(stripHtmlTags('<div>Content<div>')).toBe('Content');
	});
});

describe('htmlToMarkdown', () => {
	it('returns empty string for null input', () => {
		expect(htmlToMarkdown(null)).toBe('');
	});
	it('returns empty string for undefined input', () => {
		expect(htmlToMarkdown(undefined)).toBe('');
	});
	it('returns empty string for empty string input', () => {
		expect(htmlToMarkdown('')).toBe('');
	});
	it('converts paragraph tags', () => {
		expect(htmlToMarkdown('<p>First paragraph</p><p>Second paragraph</p>')).toBe('First paragraph\n\nSecond paragraph');
	});
	it('converts br tags to newlines', () => {
		expect(htmlToMarkdown('Line one<br>Line two')).toBe('Line one\nLine two');
		expect(htmlToMarkdown('Line one<br/>Line two')).toBe('Line one\nLine two');
		expect(htmlToMarkdown('Line one<br />Line two')).toBe('Line one\nLine two');
	});
	it('converts heading tags to bold', () => {
		expect(htmlToMarkdown('<h1>Heading</h1>')).toBe('**Heading**');
		expect(htmlToMarkdown('<h2>Heading</h2>')).toBe('**Heading**');
		expect(htmlToMarkdown('<h6>Heading</h6>')).toBe('**Heading**');
	});
	it('converts list items', () => {
		const html = '<ul><li>Item 1</li><li>Item 2</li></ul>';
		const result = htmlToMarkdown(html);
		expect(result).toContain('Item 1');
		expect(result).toContain('Item 2');
	});
	it('converts code blocks', () => {
		expect(htmlToMarkdown('<pre><code>const x = 1;</code></pre>')).toContain('```\nconst x = 1;\n```');
	});
	it('converts inline code', () => {
		expect(htmlToMarkdown('Use <code>npm install</code> to install')).toBe('Use `npm install` to install');
	});
	it('converts bold tags', () => {
		expect(htmlToMarkdown('<strong>Bold text</strong>')).toBe('**Bold text**');
		expect(htmlToMarkdown('<b>Bold text</b>')).toBe('**Bold text**');
	});
	it('converts italic tags', () => {
		expect(htmlToMarkdown('<em>Italic text</em>')).toBe('_Italic text_');
		expect(htmlToMarkdown('<i>Italic text</i>')).toBe('_Italic text_');
	});
	it('converts links', () => {
		expect(htmlToMarkdown('<a href="https://example.com">Example</a>')).toBe('[Example](https://example.com)');
	});
	it('collapses multiple newlines', () => {
		expect(htmlToMarkdown('<p>A</p><p></p><p></p><p>B</p>')).toBe('A\n\nB');
	});
	it('trims whitespace', () => {
		expect(htmlToMarkdown('  <p>Content</p>  ')).toBe('Content');
	});
	it('decodes HTML entities in the result', () => {
		expect(htmlToMarkdown('<p>Hello &amp; World</p>')).toBe('Hello & World');
	});
	it('handles complex mixed HTML', () => {
		const html =
			'<p>This is <strong>bold</strong> and <em>italic</em> with a <a href="https://example.com">link</a>.</p>';
		const expected = 'This is **bold** and _italic_ with a [link](https://example.com).';
		expect(htmlToMarkdown(html)).toBe(expected);
	});
});
