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

import {describe, expect, test} from 'vitest';
import {NodeType} from '../types/enums';
import type {FormattingNode, Node, TextNode} from '../types/nodes';
import {addTextNode, combineAdjacentTextNodes, flattenSameType, isFormattingNode, mergeTextNodes} from './ast-utils';

describe('AST Utils', () => {
	describe('isFormattingNode', () => {
		test('should identify formatting nodes', () => {
			const emphasisNode: FormattingNode = {
				type: NodeType.Emphasis,
				children: [{type: NodeType.Text, content: 'test'}],
			};
			const strongNode: FormattingNode = {
				type: NodeType.Strong,
				children: [{type: NodeType.Text, content: 'test'}],
			};
			const textNode: TextNode = {
				type: NodeType.Text,
				content: 'test',
			};

			expect(isFormattingNode(emphasisNode)).toBe(true);
			expect(isFormattingNode(strongNode)).toBe(true);
			expect(isFormattingNode(textNode)).toBe(false);
		});
	});

	describe('flattenSameType', () => {
		test('should flatten nodes of the same type', () => {
			const children: Array<Node> = [
				{type: NodeType.Text, content: 'first'},
				{
					type: NodeType.Emphasis,
					children: [
						{type: NodeType.Text, content: 'nested1'},
						{type: NodeType.Text, content: 'nested2'},
					],
				},
				{type: NodeType.Text, content: 'last'},
			];

			flattenSameType(children, NodeType.Emphasis);
			expect(children).toHaveLength(4);
		});

		test('should handle empty children arrays', () => {
			const children: Array<Node> = [];
			flattenSameType(children, NodeType.Text);
			expect(children).toEqual([]);
		});
	});

	describe('combineAdjacentTextNodes', () => {
		test('should combine adjacent text nodes', () => {
			const nodes: Array<Node> = [
				{type: NodeType.Text, content: 'first'},
				{type: NodeType.Text, content: 'second'},
				{type: NodeType.Text, content: 'third'},
			];

			combineAdjacentTextNodes(nodes);
			expect(nodes).toHaveLength(1);
			expect(nodes[0]).toEqual({type: NodeType.Text, content: 'firstsecondthird'});
		});

		test('should not combine non-text nodes', () => {
			const nodes: Array<Node> = [
				{type: NodeType.Text, content: 'text'},
				{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'emphasis'}]},
				{type: NodeType.Text, content: 'more text'},
			];
			combineAdjacentTextNodes(nodes);

			expect(nodes).toHaveLength(3);
			expect(nodes[0]).toEqual({type: NodeType.Text, content: 'text'});
			expect(nodes[1].type).toBe(NodeType.Emphasis);
			expect(nodes[2]).toEqual({type: NodeType.Text, content: 'more text'});
		});
	});

	describe('mergeTextNodes', () => {
		test('should merge text nodes', () => {
			const nodes: Array<Node> = [
				{type: NodeType.Text, content: 'hello '},
				{type: NodeType.Text, content: 'world'},
			];
			const result = mergeTextNodes(nodes);

			expect(result).toHaveLength(1);
			expect(result[0]).toEqual({type: NodeType.Text, content: 'hello world'});
		});

		test('should handle mixed node types', () => {
			const nodes: Array<Node> = [
				{type: NodeType.Text, content: 'start'},
				{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'middle'}]},
				{type: NodeType.Text, content: 'end'},
			];
			const result = mergeTextNodes(nodes);

			expect(result).toHaveLength(3);
			expect(result[0]).toEqual({type: NodeType.Text, content: 'start'});
			expect(result[1].type).toBe(NodeType.Emphasis);
			expect(result[2]).toEqual({type: NodeType.Text, content: 'end'});
		});

		test('should handle empty arrays', () => {
			const result = mergeTextNodes([]);
			expect(result).toEqual([]);
		});
	});

	describe('addTextNode', () => {
		test('should add text node to array', () => {
			const nodes: Array<Node> = [];
			addTextNode(nodes, 'test content');

			expect(nodes).toHaveLength(1);
			expect(nodes[0]).toEqual({type: NodeType.Text, content: 'test content'});
		});

		test('should handle empty text', () => {
			const nodes: Array<Node> = [];
			addTextNode(nodes, '');

			expect(nodes).toHaveLength(0);
		});
	});
});
