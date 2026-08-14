import type React from 'react';

declare module 'react' {
	namespace JSX {
		interface IntrinsicElements {
			[tagName: `flx-${string}`]: React.DetailedHTMLProps<React.HTMLAttributes<HTMLElement>, HTMLElement>;
		}
	}
}

declare global {
	interface HTMLElementTagNameMap {
		[tagName: `flx-${string}`]: HTMLElement;
	}
}
