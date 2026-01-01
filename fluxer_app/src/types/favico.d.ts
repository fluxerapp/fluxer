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

declare module 'favico.js' {
	export interface FavicoOptions {
		animation?: string;
		bgColor?: string;
		textColor?: string;
		fontFamily?: string;
		fontStyle?: string;
		type?: string;
		position?: string;
		element?: HTMLElement;
		elementId?: string;
		dataUrl?: (url: string) => void;
	}

	export default class Favico {
		constructor(options?: FavicoOptions);
		badge(count: number | string): void;
		reset(): void;
		image(image: HTMLImageElement | HTMLCanvasElement): void;
		video(video: HTMLVideoElement): void;
		webcam(): void;
	}
}
