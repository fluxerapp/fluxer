// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {OPERATOR_PASS_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/premium/components/operator_pass/checkout/OperatorPassShowcase.module.css';
import {OPERATOR_PASS_SHOWCASE_SLIDES} from '@app/features/premium/components/operator_pass/checkout/OperatorPassShowcaseSlides';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import WindowState from '@app/features/window/state/Window';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

const AUTOPLAY_MS = 3500;

const SHOWCASE_LABEL_DESCRIPTOR = msg({
	message: 'What comes with the {operatorPassName}',
	comment:
		'Accessible name of the slideshow on the left side of the Operator Pass checkout dialog, which shows what comes with the pass one slide at a time. Operator Pass is a product name and stays untranslated. Never use a word that means bonus, perks, gift, included or free.',
});
const SHOW_SLIDE_DESCRIPTOR = msg({
	message: 'Show {title}',
	comment:
		'Accessible label of a small dot under the slideshow in the Operator Pass checkout dialog. Selecting it shows that slide. {title} is the heading of the slide, such as The Operator badge.',
});

interface OperatorPassShowcaseProps {
	resting: boolean;
	compact: boolean;
}

export const OperatorPassShowcase = observer(function OperatorPassShowcase({
	resting,
	compact,
}: OperatorPassShowcaseProps) {
	const {i18n} = useLingui();
	const [chosen, setChosen] = useState(0);
	const [hovered, setHovered] = useState(false);
	const [focused, setFocused] = useState(false);
	const reducedMotion = Accessibility.useReducedMotion;
	const index = resting ? 0 : chosen;
	const slide = OPERATOR_PASS_SHOWCASE_SLIDES[index] ?? OPERATOR_PASS_SHOWCASE_SLIDES[0]!;
	const playing = !resting && !hovered && !focused && !reducedMotion && WindowState.visible;
	useEffect(() => {
		if (!playing) return;
		const timer = setTimeout(
			() => setChosen((current) => (current + 1) % OPERATOR_PASS_SHOWCASE_SLIDES.length),
			AUTOPLAY_MS,
		);
		return () => clearTimeout(timer);
	}, [playing, chosen]);
	const preloadNext = () => {
		const next = OPERATOR_PASS_SHOWCASE_SLIDES[(index + 1) % OPERATOR_PASS_SHOWCASE_SLIDES.length];
		if (!next) return;
		const image = new Image();
		image.src = next.avif;
	};
	const transition = reducedMotion ? {duration: 0} : {duration: 0.4, ease: 'easeOut' as const};
	return (
		<section
			className={clsx(styles.showcase, compact && styles.compact)}
			aria-roledescription="carousel"
			aria-label={i18n._(SHOWCASE_LABEL_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME})}
			onMouseEnter={() => setHovered(true)}
			onMouseLeave={() => setHovered(false)}
			onFocus={() => setFocused(true)}
			onBlur={(event) => {
				if (!event.currentTarget.contains(event.relatedTarget as Node | null)) setFocused(false);
			}}
			data-flx="premium.operator-pass-showcase.section"
		>
			<div
				className={styles.stage}
				aria-live={playing ? 'off' : 'polite'}
				data-flx="premium.operator-pass-showcase.stage"
			>
				<AnimatePresence
					mode="wait"
					initial={false}
					data-flx="premium.operator-pass.checkout.operator-pass-showcase.animate-presence"
				>
					<motion.div
						key={slide.id}
						className={styles.slide}
						initial={reducedMotion ? false : {opacity: 0, y: 12}}
						animate={{opacity: 1, y: 0}}
						exit={reducedMotion ? {opacity: 0} : {opacity: 0, y: -12}}
						transition={transition}
						aria-roledescription="slide"
						data-flx="premium.operator-pass-showcase.slide"
					>
						<picture className={styles.picture} data-flx="premium.operator-pass-showcase.picture">
							<source
								type="image/avif"
								srcSet={slide.avif}
								data-flx="premium.operator-pass.checkout.operator-pass-showcase.source.image-avif"
							/>
							<img
								className={styles.image}
								src={slide.webp}
								width={slide.width}
								height={slide.height}
								alt={slide.alt ? i18n._(slide.alt) : ''}
								decoding="async"
								draggable={false}
								onLoad={preloadNext}
								data-flx="premium.operator-pass-showcase.image"
							/>
						</picture>
						<h3 className={styles.title} data-flx="premium.operator-pass-showcase.title">
							{i18n._(slide.title)}
						</h3>
						{!compact && (
							<p className={styles.body} data-flx="premium.operator-pass-showcase.body">
								{i18n._(slide.body, {productName: PRODUCT_NAME})}
							</p>
						)}
					</motion.div>
				</AnimatePresence>
			</div>
			{!compact && (
				<div className={styles.footer} data-flx="premium.operator-pass-showcase.footer">
					<div className={styles.dots} data-flx="premium.operator-pass-showcase.dots">
						{OPERATOR_PASS_SHOWCASE_SLIDES.map((entry, entryIndex) => (
							<FocusRing key={entry.id} offset={-4} data-flx="premium.operator-pass-showcase.dot-focus-ring">
								<button
									type="button"
									className={styles.dot}
									aria-label={i18n._(SHOW_SLIDE_DESCRIPTOR, {title: i18n._(entry.title)})}
									aria-current={entryIndex === index || undefined}
									disabled={resting}
									onClick={() => setChosen(entryIndex)}
									data-flx="premium.operator-pass-showcase.dot"
								/>
							</FocusRing>
						))}
					</div>
					<p className={styles.terms} data-flx="premium.operator-pass-showcase.terms">
						<Trans comment="Muted line under the slideshow in the Operator Pass checkout dialog. The linked text opens section 3 of the Operator Pass terms, which lists everything that comes with the pass. {OPERATOR_PASS_NAME} is a product name and stays untranslated. Never use a word that means bonus, perks, gift, included or free.">
							What comes with the pass is described in the{' '}
							<ExternalLink
								href={`${Routes.operatorPassTerms()}#3-the-bonus`}
								className={styles.termsLink}
								data-flx="premium.operator-pass-showcase.terms-link"
							>
								{OPERATOR_PASS_NAME} terms
							</ExternalLink>
							.
						</Trans>
					</p>
				</div>
			)}
		</section>
	);
});
