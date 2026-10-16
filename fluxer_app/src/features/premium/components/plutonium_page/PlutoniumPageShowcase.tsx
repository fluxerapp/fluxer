// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPage.module.css';
import {
	AVATAR_LSF,
	BANNER_LSF,
	EMOJI_CATVIBE,
	EMOJI_CATWAVE,
	EXPRESSIONS_SHOTS,
	PROFILE_SHOTS,
	type ShotFamily,
	STICKER_FEEL_THAT,
	STREAM_SHOTS,
	UPLOAD_SHOTS,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageMedia';
import {
	SHOWCASE_EXPRESSIONS_BODY_DESCRIPTOR,
	SHOWCASE_EXPRESSIONS_TITLE_DESCRIPTOR,
	SHOWCASE_PROFILE_BODY_DESCRIPTOR,
	SHOWCASE_PROFILE_BODY_WITHOUT_TAG_DESCRIPTOR,
	SHOWCASE_PROFILE_TITLE_DESCRIPTOR,
	SHOWCASE_STREAM_BODY_DESCRIPTOR,
	SHOWCASE_STREAM_TITLE_DESCRIPTOR,
	SHOWCASE_UPLOAD_BODY_DESCRIPTOR,
	SHOWCASE_UPLOAD_TITLE_DESCRIPTOR,
	TAG_FOOTNOTE_MARKER_DESCRIPTOR,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageMessages';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {MessageDescriptor} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';

const PERK_SHOT_SIZES = '(min-width: 48rem) 50vw, calc(100vw - 5rem)';
const CHAT_SURFACE = 'rgb(30, 29, 35)';

interface PerkOverlay {
	src: string;
	left: number;
	top: number;
	width: number;
	height: number;
	backdrop?: string;
}

interface PerkCard {
	id: string;
	title: MessageDescriptor;
	body: MessageDescriptor;
	shots: ShotFamily;
	width: number;
	height: number;
	overlays: ReadonlyArray<PerkOverlay>;
	profileOverlay?: boolean;
}

const SHOT_WIDTHS = [640, 1120, 1600] as const;

const EXPRESSIONS_OVERLAYS: ReadonlyArray<PerkOverlay> = [
	{src: EMOJI_CATVIBE, left: 65.22, top: 11.241, width: 4.372, height: 5.128, backdrop: CHAT_SURFACE},
	{src: EMOJI_CATWAVE, left: 14.936, top: 31.197, width: 8.743, height: 10.256, backdrop: CHAT_SURFACE},
	{src: STICKER_FEEL_THAT, left: 14.936, top: 52.35, width: 29.144, height: 34.188},
];

const PERK_CARDS: ReadonlyArray<PerkCard> = [
	{
		id: 'expressions',
		title: SHOWCASE_EXPRESSIONS_TITLE_DESCRIPTOR,
		body: SHOWCASE_EXPRESSIONS_BODY_DESCRIPTOR,
		shots: EXPRESSIONS_SHOTS,
		width: 549,
		height: 468,
		overlays: EXPRESSIONS_OVERLAYS,
	},
	{
		id: 'profile',
		title: SHOWCASE_PROFILE_TITLE_DESCRIPTOR,
		body: SHOWCASE_PROFILE_BODY_DESCRIPTOR,
		shots: PROFILE_SHOTS,
		width: 662,
		height: 407,
		overlays: [],
		profileOverlay: true,
	},
	{
		id: 'stream',
		title: SHOWCASE_STREAM_TITLE_DESCRIPTOR,
		body: SHOWCASE_STREAM_BODY_DESCRIPTOR,
		shots: STREAM_SHOTS,
		width: 588,
		height: 343,
		overlays: [],
	},
	{
		id: 'upload',
		title: SHOWCASE_UPLOAD_TITLE_DESCRIPTOR,
		body: SHOWCASE_UPLOAD_BODY_DESCRIPTOR,
		shots: UPLOAD_SHOTS,
		width: 549,
		height: 362,
		overlays: [],
	},
];

function srcSet(urls: ReadonlyArray<string>): string {
	return urls.map((url, index) => `${url} ${SHOT_WIDTHS[index]}w`).join(', ');
}

function ProfileOverlay() {
	return (
		<svg
			className={styles.perkOverlaySvg}
			viewBox="0 0 662 407"
			aria-hidden="true"
			focusable="false"
			data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.perk-overlay-svg"
		>
			<defs data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.defs">
				<clipPath
					id="plutonium-perk-banner-clip"
					data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.plutonium-perk-banner-clip"
				>
					<path
						d="M345.333 121.448V19.833a3.5 3.5 0 0 1 3.5-3.5h293.333a3.5 3.5 0 0 1 3.5 3.5v101.615z"
						data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.path"
					/>
				</clipPath>
				<mask
					id="plutonium-perk-banner-bite"
					maskUnits="userSpaceOnUse"
					x="0"
					y="0"
					width="662"
					height="407"
					data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.plutonium-perk-banner-bite"
				>
					<rect
						width="662"
						height="407"
						fill="#fff"
						data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.rect"
					/>
					<circle
						cx="398.333"
						cy="114.333"
						r="43.2"
						fill="#000"
						data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.circle"
					/>
				</mask>
				<clipPath
					id="plutonium-perk-avatar-clip"
					data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.plutonium-perk-avatar-clip"
				>
					<circle
						cx="398.333"
						cy="114.333"
						r="40"
						data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.circle--2"
					/>
				</clipPath>
				<mask
					id="plutonium-perk-avatar-notch"
					maskUnits="userSpaceOnUse"
					x="0"
					y="0"
					width="662"
					height="407"
					data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.plutonium-perk-avatar-notch"
				>
					<rect
						width="662"
						height="407"
						fill="#fff"
						data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.rect--2"
					/>
					<circle
						cx="426.333"
						cy="142.333"
						r="11.2"
						fill="#000"
						data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.circle--3"
					/>
				</mask>
			</defs>
			<g
				clipPath="url(#plutonium-perk-banner-clip)"
				data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.g"
			>
				<g
					mask="url(#plutonium-perk-banner-bite)"
					data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.g--2"
				>
					<image
						href={BANNER_LSF}
						x="345.333"
						y="16.333"
						width="300.333"
						height="105.115"
						preserveAspectRatio="xMidYMid slice"
						data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.image"
					/>
				</g>
				<circle
					cx="398.333"
					cy="114.333"
					r="41.6"
					fill="none"
					stroke="#0c0b0e"
					strokeWidth="3.2"
					data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.circle--4"
				/>
			</g>
			<g
				clipPath="url(#plutonium-perk-avatar-clip)"
				mask="url(#plutonium-perk-avatar-notch)"
				data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.g--3"
			>
				<image
					href={AVATAR_LSF}
					x="358.333"
					y="74.333"
					width="80"
					height="80"
					preserveAspectRatio="xMidYMid slice"
					data-flx="premium.plutonium-page.plutonium-page-showcase.profile-overlay.image--2"
				/>
			</g>
		</svg>
	);
}

function PerkArt({card, eager}: {card: PerkCard; eager: boolean}) {
	return (
		<div className={styles.perkArt} data-flx="premium.plutonium-page.perk-art">
			<picture data-flx="premium.plutonium-page.plutonium-page-showcase.perk-art.picture">
				<source
					type="image/avif"
					srcSet={srcSet(card.shots.avif)}
					sizes={PERK_SHOT_SIZES}
					data-flx="premium.plutonium-page.plutonium-page-showcase.perk-art.source.image-avif"
				/>
				<source
					type="image/webp"
					srcSet={srcSet(card.shots.webp)}
					sizes={PERK_SHOT_SIZES}
					data-flx="premium.plutonium-page.plutonium-page-showcase.perk-art.source.image-webp"
				/>
				<img
					draggable={false}
					className={styles.perkArtImage}
					src={card.shots.webp[1]}
					width={card.width}
					height={card.height}
					loading={eager ? 'eager' : 'lazy'}
					fetchPriority={eager ? 'high' : undefined}
					decoding="async"
					alt=""
					data-flx="premium.plutonium-page.perk-art.image"
				/>
			</picture>
			{card.overlays.map((overlay) => (
				<img
					key={overlay.src}
					draggable={false}
					className={styles.perkOverlay}
					src={overlay.src}
					alt=""
					aria-hidden="true"
					loading="lazy"
					decoding="async"
					fetchPriority="low"
					style={{
						left: `${overlay.left.toFixed(3)}%`,
						top: `${overlay.top.toFixed(3)}%`,
						width: `${overlay.width.toFixed(3)}%`,
						height: `${overlay.height.toFixed(3)}%`,
						borderRadius: 0,
						backgroundColor: overlay.backdrop,
					}}
					data-flx="premium.plutonium-page.perk-art.overlay"
				/>
			))}
			{card.profileOverlay && (
				<ProfileOverlay data-flx="premium.plutonium-page.plutonium-page-showcase.perk-art.profile-overlay" />
			)}
		</div>
	);
}

interface PlutoniumPageShowcaseProps {
	uploadSize: string;
	freeUploadSize: string;
	footnoteId: string;
	onFootnoteClick: (event: React.MouseEvent<HTMLAnchorElement>) => void;
}

export function PlutoniumPageShowcase({
	uploadSize,
	freeUploadSize,
	footnoteId,
	onFootnoteClick,
}: PlutoniumPageShowcaseProps) {
	const {i18n} = useLingui();
	const showArt = !RuntimeConfig.isSelfHosted();
	return (
		<>
			{PERK_CARDS.map((card, index) => {
				const flipped = index % 2 === 1;
				const body =
					card.id === 'profile' && RuntimeConfig.usesUniqueUsernames
						? i18n._(SHOWCASE_PROFILE_BODY_WITHOUT_TAG_DESCRIPTOR)
						: card.id === 'profile'
							? renderProfileBody(
									i18n._(card.body, {footnote: '\u0000'}),
									footnoteId,
									onFootnoteClick,
									i18n._(TAG_FOOTNOTE_MARKER_DESCRIPTOR),
								)
							: i18n._(card.body, {freeSize: freeUploadSize});
				return (
					<article
						key={card.id}
						className={clsx(styles.perkRow, showArt ? flipped && styles.perkRowFlipped : styles.perkRowTextOnly)}
						data-flx={`premium.plutonium-page.perk-row.${card.id}`}
					>
						{showArt && (
							<div className={styles.perkArtColumn} data-flx="premium.plutonium-page.perk-row.art-column">
								<PerkArt
									card={card}
									eager={index === 0}
									data-flx="premium.plutonium-page.plutonium-page-showcase.perk-art"
								/>
							</div>
						)}
						<div className={styles.perkTextColumn} data-flx="premium.plutonium-page.perk-row.text-column">
							<h2 className={styles.perkTitle} data-flx="premium.plutonium-page.perk-row.title">
								{i18n._(card.title, {size: uploadSize})}
							</h2>
							<p className={styles.perkBody} data-flx="premium.plutonium-page.perk-row.body">
								{body}
							</p>
						</div>
					</article>
				);
			})}
		</>
	);
}

function renderProfileBody(
	text: string,
	footnoteId: string,
	onFootnoteClick: (event: React.MouseEvent<HTMLAnchorElement>) => void,
	markerLabel: string,
): React.ReactNode {
	const [before, after = ''] = text.split('\u0000');
	return (
		<>
			<span data-flx="premium.plutonium-page.plutonium-page-showcase.render-profile-body.span">{before}</span>
			<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page-showcase.render-profile-body.focus-ring">
				<a
					href={`#${footnoteId}`}
					className={styles.footnoteMarker}
					onClick={onFootnoteClick}
					data-flx="premium.plutonium-page.perk-row.footnote-marker"
				>
					<span
						aria-hidden="true"
						data-flx="premium.plutonium-page.plutonium-page-showcase.render-profile-body.span--2"
					>
						*
					</span>
					<span
						className={styles.srOnly}
						data-flx="premium.plutonium-page.plutonium-page-showcase.render-profile-body.sr-only"
					>
						{markerLabel}
					</span>
				</a>
			</FocusRing>
			<span data-flx="premium.plutonium-page.plutonium-page-showcase.render-profile-body.span--3">{after}</span>
		</>
	);
}
