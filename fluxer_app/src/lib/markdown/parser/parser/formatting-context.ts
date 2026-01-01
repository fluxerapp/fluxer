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

import {MAX_INLINE_DEPTH} from '../types/constants';

export class FormattingContext {
	private readonly activeFormattingTypes = new Map<string, boolean>();
	private readonly formattingStack: Array<[string, boolean]> = [];
	private currentDepth = 0;

	canEnterFormatting(delimiter: string, isDouble: boolean): boolean {
		const key = this.getFormattingKey(delimiter, isDouble);
		if (this.activeFormattingTypes.has(key)) return false;
		return this.currentDepth < MAX_INLINE_DEPTH;
	}

	isFormattingActive(delimiter: string, isDouble: boolean): boolean {
		return this.activeFormattingTypes.has(this.getFormattingKey(delimiter, isDouble));
	}

	pushFormatting(delimiter: string, isDouble: boolean): void {
		this.formattingStack.push([delimiter, isDouble]);
		this.activeFormattingTypes.set(this.getFormattingKey(delimiter, isDouble), true);
		this.currentDepth++;
	}

	popFormatting(): [string, boolean] | undefined {
		const removed = this.formattingStack.pop();
		if (removed) {
			this.activeFormattingTypes.delete(this.getFormattingKey(removed[0], removed[1]));
			this.currentDepth--;
		}
		return removed;
	}

	setCurrentText(_text: string): void {}

	private getFormattingKey(delimiter: string, isDouble: boolean): string {
		return `${delimiter}${isDouble ? '2' : '1'}`;
	}
}
