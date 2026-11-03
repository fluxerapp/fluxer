// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/plutonium_page/PlutoniumPage.module.css';
import {clsx} from 'clsx';
import type React from 'react';

type ShapeKind = 'spark' | 'ring' | 'dot' | 'plus';

interface Shape {
	kind: ShapeKind;
	x?: number;
	xe?: number;
	y: number;
	size: string;
}

const SHAPES: ReadonlyArray<Shape> = [
	{kind: 'spark', x: 0.18, y: 6, size: 'max(2.5rem, 1.6vw)'},
	{kind: 'ring', x: 0.55, y: 24, size: 'max(3rem, 1.9vw)'},
	{kind: 'dot', x: 0.72, y: 8, size: 'max(1rem, 0.7vw)'},
	{kind: 'plus', x: 0.3, y: 44, size: 'max(1.75rem, 1.1vw)'},
	{kind: 'spark', x: 0.7, y: 58, size: 'max(1.5rem, 1vw)'},
	{kind: 'dot', x: 0.12, y: 66, size: 'max(0.75rem, 0.5vw)'},
	{kind: 'plus', x: 0.48, y: 80, size: 'max(1.5rem, 1vw)'},
	{kind: 'ring', xe: 0.2, y: 4, size: 'max(2.25rem, 1.5vw)'},
	{kind: 'plus', xe: 0.62, y: 16, size: 'max(1.5rem, 1vw)'},
	{kind: 'dot', xe: 0.78, y: 34, size: 'max(1.25rem, 0.8vw)'},
	{kind: 'spark', xe: 0.3, y: 40, size: 'max(3rem, 1.9vw)'},
	{kind: 'dot', xe: 0.6, y: 60, size: 'max(0.875rem, 0.6vw)'},
	{kind: 'ring', xe: 0.16, y: 70, size: 'max(1.75rem, 1.1vw)'},
	{kind: 'plus', xe: 0.5, y: 84, size: 'max(2rem, 1.3vw)'},
];

function ShapeGlyph({kind}: {kind: ShapeKind}) {
	switch (kind) {
		case 'ring':
			return (
				<svg viewBox="0 0 40 40" data-flx="premium.plutonium-page.hero-shapes.ring">
					<circle
						cx="20"
						cy="20"
						r="15"
						fill="none"
						stroke="currentColor"
						strokeWidth="6"
						data-flx="premium.plutonium-page.hero-shapes.ring-circle"
					/>
				</svg>
			);
		case 'spark':
			return (
				<svg viewBox="0 0 40 40" data-flx="premium.plutonium-page.hero-shapes.spark">
					<path
						d="M20 0C21.5 12 28 18.5 40 20C28 21.5 21.5 28 20 40C18.5 28 12 21.5 0 20C12 18.5 18.5 12 20 0Z"
						fill="currentColor"
						data-flx="premium.plutonium-page.hero-shapes.spark-path"
					/>
				</svg>
			);
		case 'plus':
			return (
				<svg viewBox="0 0 40 40" data-flx="premium.plutonium-page.hero-shapes.plus">
					<path
						d="M15 2h10v13h13v10H25v13H15V25H2V15h13z"
						fill="currentColor"
						data-flx="premium.plutonium-page.hero-shapes.plus-path"
					/>
				</svg>
			);
		default:
			return (
				<svg viewBox="0 0 40 40" data-flx="premium.plutonium-page.hero-shapes.dot">
					<circle cx="20" cy="20" r="20" fill="currentColor" data-flx="premium.plutonium-page.hero-shapes.dot-circle" />
				</svg>
			);
	}
}

function shapeStyle(shape: Shape): React.CSSProperties {
	const offset = (factor: number) => `calc((var(--gutter) - var(--s)) * ${factor})`;
	return {
		'--x': shape.x != null ? offset(shape.x) : 'auto',
		'--xe': shape.xe != null ? offset(shape.xe) : 'auto',
		'--y': `${shape.y}%`,
		'--s': shape.size,
	} as React.CSSProperties;
}

export function PlutoniumPageHeroShapes() {
	return (
		<div className={styles.heroShapes} aria-hidden="true" data-flx="premium.plutonium-page.hero-shapes">
			{SHAPES.map((shape, index) => (
				<span
					key={`${shape.kind}-${index}`}
					className={clsx(styles.shape, shape.kind === 'spark' && styles.shapeSpark)}
					style={shapeStyle(shape)}
					data-flx="premium.plutonium-page.hero-shapes.shape"
				>
					<ShapeGlyph kind={shape.kind} data-flx="premium.plutonium-page.hero-shapes.glyph" />
				</span>
			))}
		</div>
	);
}
