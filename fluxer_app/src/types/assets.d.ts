// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FunctionComponent, SVGProps} from 'react';
import type {Messages} from '@lingui/core';

declare module '*.css' {}

declare module '*.po' {
	export const messages: Messages;
}

declare module '*.svg' {
	const url: string;
	export default url;
}

declare module '*.svg?react' {
	const ReactComponent: FunctionComponent<SVGProps<SVGSVGElement>>;
	export default ReactComponent;
}

declare module '*.mp4' {
	const url: string;
	export default url;
}

declare module '*.webm' {
	const url: string;
	export default url;
}

declare module '*.png' {
	const url: string;
	export default url;
}

declare module '*.jpg' {
	const url: string;
	export default url;
}

declare module '*.jpeg' {
	const url: string;
	export default url;
}

declare module '*.gif' {
	const url: string;
	export default url;
}

declare module '*.webp' {
	const url: string;
	export default url;
}

declare module '@pkgs/libfluxcore/libfluxcore_bg.wasm' {
	const url: string;
	export default url;
}

declare module '*.onnx' {
	const url: string;
	export default url;
}
