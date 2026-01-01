/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {describe, expect, it} from 'vitest';
import {decodeHTMLEntities, htmlToMarkdown, stripHtmlTags} from './DOMUtils';

describe('DOMUtils', () => {
	describe('decodeHTMLEntities', () => {
		it('should decode common HTML entities', () => {
			expect(decodeHTMLEntities('&amp;')).toBe('&');
			expect(decodeHTMLEntities('&lt;')).toBe('<');
			expect(decodeHTMLEntities('&gt;')).toBe('>');
			expect(decodeHTMLEntities('&quot;')).toBe('"');
			expect(decodeHTMLEntities('&#39;')).toBe("'");
		});

		it('should handle empty or null input', () => {
			expect(decodeHTMLEntities('')).toBe('');
			expect(decodeHTMLEntities(null)).toBe('');
			expect(decodeHTMLEntities(undefined)).toBe('');
		});

		it('should decode numeric entities', () => {
			expect(decodeHTMLEntities('&#65;')).toBe('A');
			expect(decodeHTMLEntities('&#8364;')).toBe('€');
		});

		it('should handle text without entities', () => {
			expect(decodeHTMLEntities('Hello World')).toBe('Hello World');
		});

		it('should decode mixed content', () => {
			expect(decodeHTMLEntities('Hello &amp; goodbye &lt;script&gt;')).toBe('Hello & goodbye <script>');
		});
	});

	describe('stripHtmlTags', () => {
		it('should remove simple HTML tags', () => {
			expect(stripHtmlTags('<p>Hello</p>')).toBe('Hello');
			expect(stripHtmlTags('<div>World</div>')).toBe('World');
		});

		it('should handle empty or null input', () => {
			expect(stripHtmlTags('')).toBe('');
			expect(stripHtmlTags(null)).toBe('');
			expect(stripHtmlTags(undefined)).toBe('');
		});

		it('should remove nested tags', () => {
			expect(stripHtmlTags('<div><p>Hello <strong>World</strong></p></div>')).toBe('Hello World');
		});

		it('should remove self-closing tags', () => {
			expect(stripHtmlTags('Line 1<br/>Line 2<hr/>')).toBe('Line 1Line 2');
		});

		it('should handle tags with attributes', () => {
			expect(stripHtmlTags('<a href="http://example.com">Link</a>')).toBe('Link');
			expect(stripHtmlTags('<img src="image.jpg" alt="Image"/>')).toBe('');
		});

		it('should handle malformed tags', () => {
			expect(stripHtmlTags('<p>Hello <world')).toBe('Hello <world');
			expect(stripHtmlTags('Hello > World')).toBe('Hello > World');
		});

		it('should preserve text content', () => {
			expect(stripHtmlTags('No tags here')).toBe('No tags here');
		});
	});

	describe('htmlToMarkdown', () => {
		it('should handle empty or null input', () => {
			expect(htmlToMarkdown('')).toBe('');
			expect(htmlToMarkdown(null)).toBe('');
			expect(htmlToMarkdown(undefined)).toBe('');
		});

		it('should convert paragraphs', () => {
			expect(htmlToMarkdown('<p>First paragraph</p><p>Second paragraph</p>')).toBe(
				'First paragraph\n\nSecond paragraph',
			);
		});

		it('should convert line breaks', () => {
			expect(htmlToMarkdown('Line 1<br>Line 2<br/>Line 3')).toBe('Line 1\nLine 2\nLine 3');
		});

		it('should convert headings', () => {
			expect(htmlToMarkdown('<h1>Title</h1>')).toBe('**Title**');
			expect(htmlToMarkdown('<h2>Subtitle</h2>')).toBe('**Subtitle**');
			expect(htmlToMarkdown('<h6>Small heading</h6>')).toBe('**Small heading**');
		});

		it('should convert lists', () => {
			const html = '<ul><li>Item 1</li><li>Item 2</li></ul>';
			const expected = '• Item 1\n• Item 2';
			expect(htmlToMarkdown(html).trim()).toBe(expected);
		});

		it('should convert ordered lists', () => {
			const html = '<ol><li>Item 1</li><li>Item 2</li></ol>';
			const expected = '• Item 1\n• Item 2';
			expect(htmlToMarkdown(html).trim()).toBe(expected);
		});

		it('should convert code blocks', () => {
			const html = '<pre><code>console.log("hello");</code></pre>';
			const expected = '```\nconsole.log("hello");\n```';
			expect(htmlToMarkdown(html)).toBe(expected);
		});

		it('should convert inline code', () => {
			const html = 'Use <code>console.log</code> to debug';
			const expected = 'Use `console.log` to debug';
			expect(htmlToMarkdown(html)).toBe(expected);
		});

		it('should convert bold text', () => {
			expect(htmlToMarkdown('<strong>Bold text</strong>')).toBe('**Bold text**');
			expect(htmlToMarkdown('<b>Bold text</b>')).toBe('**Bold text**');
		});

		it('should convert italic text', () => {
			expect(htmlToMarkdown('<em>Italic text</em>')).toBe('_Italic text_');
			expect(htmlToMarkdown('<i>Italic text</i>')).toBe('_Italic text_');
		});

		it('should convert links', () => {
			const html = '<a href="https://example.com">Link text</a>';
			const expected = '[Link text](https://example.com)';
			expect(htmlToMarkdown(html)).toBe(expected);
		});

		it('should convert complex links with attributes', () => {
			const html = '<a class="link" href="https://example.com" target="_blank">Link text</a>';
			const expected = '[Link text](https://example.com)';
			expect(htmlToMarkdown(html)).toBe(expected);
		});

		it('should handle nested formatting', () => {
			const html = '<p><strong>Bold <em>and italic</em></strong></p>';
			const expected = '**Bold _and italic_**';
			expect(htmlToMarkdown(html)).toBe(expected);
		});

		it('should decode HTML entities', () => {
			const html = '<p>&amp;lt;script&amp;gt;</p>';
			const expected = '&lt;script&gt;';
			expect(htmlToMarkdown(html)).toBe(expected);
		});

		it('should clean up excessive newlines', () => {
			const html = '<p>Para 1</p><p></p><p></p><p>Para 2</p>';
			const result = htmlToMarkdown(html);
			expect(result).not.toContain('\n\n\n');
		});

		it('should trim whitespace', () => {
			const html = '  <p>Content</p>  ';
			expect(htmlToMarkdown(html)).toBe('Content');
		});

		it('should handle mixed content', () => {
			const html = `
				<h1>Title</h1>
				<p>This is a <strong>paragraph</strong> with <em>formatting</em>.</p>
				<ul>
					<li>List item 1</li>
					<li>List item 2</li>
				</ul>
				<p>Check out <a href="https://example.com">this link</a>.</p>
				<pre><code>const x = 42;</code></pre>
			`;

			const result = htmlToMarkdown(html);

			expect(result).toContain('**Title**');
			expect(result).toContain('**paragraph**');
			expect(result).toContain('_formatting_');
			expect(result).toContain('• List item 1');
			expect(result).toContain('[this link](https://example.com)');
			expect(result).toContain('```\nconst x = 42;\n```');
		});
	});
});
