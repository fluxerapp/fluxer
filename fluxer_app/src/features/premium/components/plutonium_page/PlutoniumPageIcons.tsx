// SPDX-License-Identifier: AGPL-3.0-or-later

export type PlutoniumPageIconName =
	| 'arrowRight'
	| 'atom'
	| 'bookmark'
	| 'chatCenteredText'
	| 'check'
	| 'cross'
	| 'gif'
	| 'globe'
	| 'hash'
	| 'heart'
	| 'image'
	| 'images'
	| 'palette'
	| 'paperclip'
	| 'rocket'
	| 'smiley'
	| 'userCircle'
	| 'usersThree'
	| 'videoCamera';

type IconBody = {kind: 'stroke'; inner: React.ReactNode} | {kind: 'fill'; path: string};

const ICONS: Record<PlutoniumPageIconName, IconBody> = {
	arrowRight: {
		kind: 'stroke',
		inner: (
			<>
				<line x1="40" y1="128" x2="216" y2="128" data-flx="premium.plutonium-page.plutonium-page-icons.line" />
				<polyline points="144,56 216,128 144,200" data-flx="premium.plutonium-page.plutonium-page-icons.polyline" />
			</>
		),
	},
	atom: {
		kind: 'stroke',
		inner: (
			<>
				<ellipse
					cx="128"
					cy="128"
					rx="96"
					ry="40"
					strokeWidth="18"
					transform="rotate(90 128 128)"
					data-flx="premium.plutonium-page.plutonium-page-icons.ellipse"
				/>
				<ellipse
					cx="128"
					cy="128"
					rx="96"
					ry="40"
					strokeWidth="18"
					transform="rotate(30 128 128)"
					data-flx="premium.plutonium-page.plutonium-page-icons.ellipse--2"
				/>
				<ellipse
					cx="128"
					cy="128"
					rx="96"
					ry="40"
					strokeWidth="18"
					transform="rotate(150 128 128)"
					data-flx="premium.plutonium-page.plutonium-page-icons.ellipse--3"
				/>
				<circle
					cx="128"
					cy="128"
					r="24"
					fill="currentColor"
					stroke="none"
					data-flx="premium.plutonium-page.plutonium-page-icons.circle"
				/>
			</>
		),
	},
	bookmark: {
		kind: 'fill',
		path: 'M184,32H72A16,16,0,0,0,56,48V224a8,8,0,0,0,12.24,6.78L128,193.43l59.77,37.35A8,8,0,0,0,200,224V48A16,16,0,0,0,184,32Z',
	},
	chatCenteredText: {
		kind: 'fill',
		path: 'M216,40H40A16,16,0,0,0,24,56V184a16,16,0,0,0,16,16h60.43l13.68,23.94a16,16,0,0,0,27.78,0L155.57,200H216a16,16,0,0,0,16-16V56A16,16,0,0,0,216,40ZM160,144H96a8,8,0,0,1,0-16h64a8,8,0,0,1,0,16Zm0-32H96a8,8,0,0,1,0-16h64a8,8,0,0,1,0,16Z',
	},
	check: {
		kind: 'stroke',
		inner: (
			<polyline points="40,144 96,200 224,72" data-flx="premium.plutonium-page.plutonium-page-icons.polyline--2" />
		),
	},
	cross: {
		kind: 'fill',
		path: 'M208.49,191.51a12,12,0,0,1-17,17L128,145,64.49,208.49a12,12,0,0,1-17-17L111,128,47.51,64.49a12,12,0,0,1,17-17L128,111l63.51-63.52a12,12,0,0,1,17,17L145,128Z',
	},
	gif: {
		kind: 'fill',
		path: 'M216,40H40A16,16,0,0,0,24,56V200a16,16,0,0,0,16,16H216a16,16,0,0,0,16-16V56A16,16,0,0,0,216,40ZM112,144a32,32,0,0,1-64,0V112a32,32,0,0,1,55.85-21.33,8,8,0,1,1-11.92,10.66A16,16,0,0,0,64,112v32a16,16,0,0,0,32,0v-8H88a8,8,0,0,1,0-16h16a8,8,0,0,1,8,8Zm32,24a8,8,0,0,1-16,0V88a8,8,0,0,1,16,0Zm60-72H176v24h20a8,8,0,0,1,0,16H176v32a8,8,0,0,1-16,0V88a8,8,0,0,1,8-8h36a8,8,0,0,1,0,16Z',
	},
	globe: {
		kind: 'fill',
		path: 'M128,24h0A104,104,0,1,0,232,128,104.12,104.12,0,0,0,128,24Zm78.36,64H170.71a135.28,135.28,0,0,0-22.3-45.6A88.29,88.29,0,0,1,206.37,88ZM216,128a87.61,87.61,0,0,1-3.33,24H174.16a157.44,157.44,0,0,0,0-48h38.51A87.61,87.61,0,0,1,216,128ZM128,43a115.27,115.27,0,0,1,26,45H102A115.11,115.11,0,0,1,128,43ZM102,168H154a115.11,115.11,0,0,1-26,45A115.27,115.27,0,0,1,102,168Zm-3.9-16a140.84,140.84,0,0,1,0-48h59.88a140.84,140.84,0,0,1,0,48Zm50.35,61.6a135.28,135.28,0,0,0,22.3-45.6h35.66A88.29,88.29,0,0,1,148.41,213.6Z',
	},
	hash: {
		kind: 'stroke',
		inner: (
			<>
				<line x1="48" y1="96" x2="224" y2="96" data-flx="premium.plutonium-page.plutonium-page-icons.line--2" />
				<line x1="176" y1="40" x2="144" y2="216" data-flx="premium.plutonium-page.plutonium-page-icons.line--3" />
				<line x1="112" y1="40" x2="80" y2="216" data-flx="premium.plutonium-page.plutonium-page-icons.line--4" />
				<line x1="32" y1="160" x2="208" y2="160" data-flx="premium.plutonium-page.plutonium-page-icons.line--5" />
			</>
		),
	},
	heart: {
		kind: 'fill',
		path: 'M240,102c0,70-103.79,126.66-108.21,129a8,8,0,0,1-7.58,0C119.79,228.66,16,172,16,102A62.07,62.07,0,0,1,78,40c20.65,0,38.73,8.88,50,23.89C139.27,48.88,157.35,40,178,40A62.07,62.07,0,0,1,240,102Z',
	},
	image: {
		kind: 'fill',
		path: 'M216,40H40A16,16,0,0,0,24,56V200a16,16,0,0,0,16,16H216a16,16,0,0,0,16-16V56A16,16,0,0,0,216,40ZM156,88a12,12,0,1,1-12,12A12,12,0,0,1,156,88Zm60,112H40V160.69l46.34-46.35a8,8,0,0,1,11.32,0h0L165,181.66a8,8,0,0,0,11.32-11.32l-17.66-17.65L173,138.34a8,8,0,0,1,11.31,0L216,170.07V200Z',
	},
	images: {
		kind: 'fill',
		path: 'M216,40H72A16,16,0,0,0,56,56V72H40A16,16,0,0,0,24,88V200a16,16,0,0,0,16,16H184a16,16,0,0,0,16-16V184h16a16,16,0,0,0,16-16V56A16,16,0,0,0,216,40ZM172,72a12,12,0,1,1-12,12A12,12,0,0,1,172,72Zm12,128H40V88H56v80a16,16,0,0,0,16,16H184Zm32-32H72V120.69l30.34-30.35a8,8,0,0,1,11.32,0L163.31,140,189,114.34a8,8,0,0,1,11.31,0L216,130.07V168Z',
	},
	palette: {
		kind: 'fill',
		path: 'M200.77,53.89A103.27,103.27,0,0,0,128,24h-1.07A104,104,0,0,0,24,128c0,43,26.58,79.06,69.36,94.17A32,32,0,0,0,136,192a16,16,0,0,1,16-16h46.21a31.81,31.81,0,0,0,31.2-24.88,104.43,104.43,0,0,0,2.59-24A103.28,103.28,0,0,0,200.77,53.89ZM84,168a12,12,0,1,1,12-12A12,12,0,0,1,84,168Zm0-56a12,12,0,1,1,12-12A12,12,0,0,1,84,112Zm44-24a12,12,0,1,1,12-12A12,12,0,0,1,128,88Zm44,24a12,12,0,1,1,12-12A12,12,0,0,1,172,112Z',
	},
	paperclip: {
		kind: 'fill',
		path: 'M212.48,136.49l-82.06,82a60,60,0,0,1-84.85-84.88l98.16-97.89a40,40,0,0,1,56.56,56.59l-.17.16-95.8,92.22a12,12,0,1,1-16.64-17.3l95.71-92.12a16,16,0,0,0-22.7-22.56L62.53,150.57a36,36,0,0,0,50.93,50.91l82.06-82a12,12,0,0,1,17,17Z',
	},
	rocket: {
		kind: 'fill',
		path: 'M152,224a8,8,0,0,1-8,8H112a8,8,0,0,1,0-16h32A8,8,0,0,1,152,224Zm71.62-68.17-12.36,55.63a16,16,0,0,1-25.51,9.11L158.51,200h-61L70.25,220.57a16,16,0,0,1-25.51-9.11L32.38,155.83a16.09,16.09,0,0,1,3.32-13.71l28.56-34.26a123.07,123.07,0,0,1,8.57-36.67c12.9-32.34,36-52.63,45.37-59.85a16,16,0,0,1,19.6,0c9.34,7.22,32.47,27.51,45.37,59.85a123.07,123.07,0,0,1,8.57,36.67l28.56,34.26A16.09,16.09,0,0,1,223.62,155.83Zm-139.23,34Q68.28,160.5,64.83,132.16L48,152.36,60.36,208l.18-.13ZM140,100a12,12,0,1,0-12,12A12,12,0,0,0,140,100Zm68,52.36-16.83-20.2q-3.42,28.28-19.56,57.69l23.85,18,.18.13Z',
	},
	smiley: {
		kind: 'fill',
		path: 'M128,24A104,104,0,1,0,232,128,104.11,104.11,0,0,0,128,24ZM92,96a12,12,0,1,1-12,12A12,12,0,0,1,92,96Zm82.92,60c-10.29,17.79-27.39,28-46.92,28s-36.63-10.2-46.92-28a8,8,0,1,1,13.84-8c7.47,12.91,19.21,20,33.08,20s25.61-7.1,33.08-20a8,8,0,1,1,13.84,8ZM164,120a12,12,0,1,1,12-12A12,12,0,0,1,164,120Z',
	},
	userCircle: {
		kind: 'fill',
		path: 'M172,120a44,44,0,1,1-44-44A44.05,44.05,0,0,1,172,120Zm60,8A104,104,0,1,1,128,24,104.11,104.11,0,0,1,232,128Zm-16,0a88.09,88.09,0,0,0-91.47-87.93C77.43,41.89,39.87,81.12,40,128.25a87.65,87.65,0,0,0,22.24,58.16A79.71,79.71,0,0,1,84,165.1a4,4,0,0,1,4.83.32,59.83,59.83,0,0,0,78.28,0,4,4,0,0,1,4.83-.32,79.71,79.71,0,0,1,21.79,21.31A87.62,87.62,0,0,0,216,128Z',
	},
	usersThree: {
		kind: 'fill',
		path: 'M64.12,147.8a4,4,0,0,1-4,4.2H16a8,8,0,0,1-7.8-6.17,8.35,8.35,0,0,1,1.62-6.93A67.79,67.79,0,0,1,37,117.51a40,40,0,1,1,66.46-35.8,3.94,3.94,0,0,1-2.27,4.18A64.08,64.08,0,0,0,64,144C64,145.28,64,146.54,64.12,147.8Zm182-8.91A67.76,67.76,0,0,0,219,117.51a40,40,0,1,0-66.46-35.8,3.94,3.94,0,0,0,2.27,4.18A64.08,64.08,0,0,1,192,144c0,1.28,0,2.54-.12,3.8a4,4,0,0,0,4,4.2H240a8,8,0,0,0,7.8-6.17A8.33,8.33,0,0,0,246.17,138.89Zm-89,43.18a48,48,0,1,0-58.37,0A72.13,72.13,0,0,0,65.07,212,8,8,0,0,0,72,224H184a8,8,0,0,0,6.93-12A72.15,72.15,0,0,0,157.19,182.07Z',
	},
	videoCamera: {
		kind: 'fill',
		path: 'M192,72V184a16,16,0,0,1-16,16H32a16,16,0,0,1-16-16V72A16,16,0,0,1,32,56H176A16,16,0,0,1,192,72Zm58,.25a8.23,8.23,0,0,0-6.63,1.22L209.78,95.86A4,4,0,0,0,208,99.19v57.62a4,4,0,0,0,1.78,3.33l33.78,22.52a8,8,0,0,0,8.58.19,8.33,8.33,0,0,0,3.86-7.17V80A8,8,0,0,0,250,72.25Z',
	},
};

export function PlutoniumPageIcon({name, className}: {name: PlutoniumPageIconName; className?: string}) {
	const icon = ICONS[name];
	if (icon.kind === 'stroke') {
		return (
			<svg
				xmlns="http://www.w3.org/2000/svg"
				viewBox="0 0 256 256"
				fill="none"
				stroke="currentColor"
				strokeLinecap="round"
				strokeLinejoin="round"
				strokeWidth="24"
				className={className}
				aria-hidden="true"
				focusable="false"
				data-flx="premium.plutonium-page.plutonium-page-icons.plutonium-page-icon.svg"
			>
				{icon.inner}
			</svg>
		);
	}
	return (
		<svg
			xmlns="http://www.w3.org/2000/svg"
			viewBox="0 0 256 256"
			className={className}
			aria-hidden="true"
			focusable="false"
			data-flx="premium.plutonium-page.plutonium-page-icons.plutonium-page-icon.svg--3"
		>
			<path
				d={icon.path}
				fill="currentColor"
				data-flx="premium.plutonium-page.plutonium-page-icons.plutonium-page-icon.path"
			/>
		</svg>
	);
}
