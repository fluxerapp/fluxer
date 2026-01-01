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

import {generate as DefaultImage} from 'fumadocs-ui/og';
import {notFound} from 'next/navigation';
import {ImageResponse} from 'next/og';
import {getPageImage, source} from '@/lib/source';

export const revalidate = false;

export async function GET(_req: Request, {params}: RouteContext<'/og/docs/[...slug]'>) {
	const {slug} = await params;
	const page = source.getPage(slug.slice(0, -1));
	if (!page) notFound();

	return new ImageResponse(<DefaultImage title={page.data.title} description={page.data.description} site="My App" />, {
		width: 1200,
		height: 630,
	});
}

export function generateStaticParams() {
	return source.getPages().map((page) => ({
		lang: page.locale,
		slug: getPageImage(page).segments,
	}));
}
