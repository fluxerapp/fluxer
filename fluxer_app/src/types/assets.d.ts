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

declare module '*.svg?react' {
	import type {FunctionComponent, SVGProps} from 'react';
	const content: FunctionComponent<SVGProps<SVGSVGElement> & {title?: string}>;
	export default content;
}

declare module '*.svg' {
	const content: string;
	export default content;
}

declare module '*.css' {
	const content: never;
	export default content;
}

declare module '*.png' {
	const content: string;
	export default content;
}

declare module '*.jpg' {
	const content: string;
	export default content;
}

declare module '*.jpeg' {
	const content: string;
	export default content;
}

declare module '*.gif' {
	const content: string;
	export default content;
}

declare module '*.webp' {
	const content: string;
	export default content;
}

declare module '*.mp3' {
	const content: string;
	export default content;
}

declare module '*.wav' {
	const content: string;
	export default content;
}

declare module '*.ogg' {
	const content: string;
	export default content;
}

declare module '*.mp4' {
	const content: string;
	export default content;
}

declare module '*.webm' {
	const content: string;
	export default content;
}

declare module '*?worker' {
	const workerConstructor: {
		new (): Worker;
	};
	export default workerConstructor;
}

declare module '*?sharedworker' {
	const sharedWorkerConstructor: {
		new (): SharedWorker;
	};
	export default sharedWorkerConstructor;
}

declare module '*.po' {
	import type {Messages} from '@lingui/core';
	export const messages: Messages;
}
