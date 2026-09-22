// SPDX-License-Identifier: AGPL-3.0-or-later

declare module '*.svg?react' {
	import type {FunctionComponent, SVGProps} from 'react';

	const content: FunctionComponent<SVGProps<SVGSVGElement>>;
	export default content;
}

declare module '*.mp4' {
	const src: string;
	export default src;
}

declare module '*.webm' {
	const src: string;
	export default src;
}

declare module '*.png' {
	const src: string;
	export default src;
}

declare module '*.jpg' {
	const src: string;
	export default src;
}

declare module '*.jpeg' {
	const src: string;
	export default src;
}

declare module '*.webp' {
	const src: string;
	export default src;
}

declare module '*?raw' {
	const content: string;
	export default content;
}
