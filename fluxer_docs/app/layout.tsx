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

import {RootProvider} from 'fumadocs-ui/provider/next';
import './global.css';
import type {Metadata, Viewport} from 'next';
import {Inter} from 'next/font/google';

const inter = Inter({
	subsets: ['latin'],
});

export const metadata: Metadata = {
	title: {
		template: 'Fluxer API Docs | %s',
		default: 'Fluxer API Docs',
	},
	description: 'Official API documentation for Fluxer',
	icons: {
		icon: [
			{url: 'https://fluxerstatic.com/web/favicon.ico'},
			{url: 'https://fluxerstatic.com/web/favicon-16x16.png', sizes: '16x16', type: 'image/png'},
			{url: 'https://fluxerstatic.com/web/favicon-32x32.png', sizes: '32x32', type: 'image/png'},
		],
		apple: {url: 'https://fluxerstatic.com/web/apple-touch-icon.png', sizes: '180x180'},
	},
};

export const viewport: Viewport = {
	themeColor: '#4641D9',
};

export default function Layout({children}: LayoutProps<'/'>) {
	return (
		<html lang="en" className={inter.className} suppressHydrationWarning>
			<body className="flex min-h-screen flex-col">
				<RootProvider>{children}</RootProvider>
			</body>
		</html>
	);
}
