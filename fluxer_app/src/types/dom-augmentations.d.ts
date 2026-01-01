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

interface HighlightRegistry {
	clear(): void;
	delete(name: string): boolean;
	entries(): IterableIterator<[string, Highlight]>;
	get(name: string): Highlight | undefined;
	has(name: string): boolean;
	keys(): IterableIterator<string>;
	set(name: string, highlight: Highlight): HighlightRegistry;
	values(): IterableIterator<Highlight>;
	[Symbol.iterator](): IterableIterator<[string, Highlight]>;
}

interface RTCStatsReport {
	entries(): IterableIterator<[string, RTCStats]>;
	get(key: string): RTCStats | undefined;
	has(key: string): boolean;
	keys(): IterableIterator<string>;
	values(): IterableIterator<RTCStats>;
	[Symbol.iterator](): IterableIterator<[string, RTCStats]>;
}
