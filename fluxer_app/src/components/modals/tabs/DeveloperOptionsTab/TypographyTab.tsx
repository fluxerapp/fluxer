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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import styles from './TypographyTab.module.css';

const fontSamples = [
	{
		fontFamily: 'IBM Plex Sans',
		name: 'IBM Plex Sans',
		sample: 'The quick brown fox jumps over the lazy dog',
		lang: 'en',
	},
	{
		fontFamily: 'IBM Plex Sans JP',
		name: 'IBM Plex Sans Japanese',
		sample: 'これは日本語のサンプルテキストです',
		lang: 'ja',
	},
	{
		fontFamily: 'IBM Plex Sans KR',
		name: 'IBM Plex Sans Korean',
		sample: '이것은 한국어 샘플 텍스트입니다',
		lang: 'ko',
	},
	{
		fontFamily: 'IBM Plex Sans SC',
		name: 'IBM Plex Sans Simplified Chinese',
		sample: '这是简体中文的示例文本',
		lang: 'zh-CN',
	},
	{
		fontFamily: 'IBM Plex Sans TC',
		name: 'IBM Plex Sans Traditional Chinese',
		sample: '這是繁體中文的示例文本',
		lang: 'zh-TW',
	},
	{
		fontFamily: 'IBM Plex Sans Arabic',
		name: 'IBM Plex Sans Arabic',
		sample: 'هذه عينة نصية باللغة العربية',
		lang: 'ar',
		rtl: true,
	},
	{
		fontFamily: 'IBM Plex Sans Hebrew',
		name: 'IBM Plex Sans Hebrew',
		sample: 'זוהי דוגמה לטקסט בעברית',
		lang: 'he',
	},
	{
		fontFamily: 'IBM Plex Sans Devanagari',
		name: 'IBM Plex Sans Devanagari',
		sample: 'यह हिंदी का नमूना पाठ है',
		lang: 'hi',
	},
	{
		fontFamily: 'IBM Plex Sans Thai',
		name: 'IBM Plex Sans Thai',
		sample: 'นี่คือข้อความตัวอย่างภาษาไทย',
		lang: 'th',
	},
	{
		fontFamily: 'IBM Plex Sans Thai Looped',
		name: 'IBM Plex Sans Thai Looped',
		sample: 'นี่คือข้อความตัวอย่างภาษาไทยแบบ Loop',
		lang: 'th',
	},
	{
		fontFamily: 'IBM Plex Mono',
		name: 'IBM Plex Mono',
		sample: 'const hello = "World"; function example() { return true; }',
		lang: 'en',
		mono: true,
	},
];

const weightExamples = [
	{weight: 100, label: 'Thin', text: 'Delicate and light typography'},
	{weight: 200, label: 'Extra Light', text: 'Gentle and airy text display'},
	{weight: 300, label: 'Light', text: 'Soft and easy reading experience'},
	{weight: 400, label: 'Regular', text: 'Perfect for everyday content'},
	{weight: 450, label: 'Text', text: 'Optimized for longer reading passages'},
	{weight: 500, label: 'Medium', text: 'Slightly bolder emphasis'},
	{weight: 600, label: 'Semi Bold', text: 'Strong visual hierarchy'},
	{weight: 700, label: 'Bold', text: 'Powerful and attention-grabbing'},
];

const scaleExamples = [
	{size: '12px', label: 'Caption', weight: 400},
	{size: '14px', label: 'Small', weight: 400},
	{size: '16px', label: 'Body', weight: 400},
	{size: '18px', label: 'Large', weight: 500},
	{size: '20px', label: 'Subtitle', weight: 500},
	{size: '24px', label: 'Heading', weight: 600},
	{size: '30px', label: 'Title', weight: 700},
	{size: '36px', label: 'Display', weight: 700},
];

const contrastExamples = [
	{weight: 400, size: '16px', style: 'Normal'},
	{weight: 400, size: '16px', style: 'Italic'},
	{weight: 600, size: '16px', style: 'Semi Bold'},
	{weight: 600, size: '18px', style: 'Semi Bold Large'},
	{weight: 700, size: '20px', style: 'Bold Heading'},
	{weight: 700, size: '24px', style: 'Bold Title'},
];

export const TypographyTabContent: React.FC = observer(() => {
	return (
		<div className={styles.container}>
			<div className={styles.section}>
				<h2 className={styles.heading}>
					<Trans>Typography Showcase</Trans>
				</h2>
				<p className={styles.description}>
					<Trans>
						Preview all available fonts, weights, and styles across different languages supported by Fluxer.
					</Trans>
				</p>
			</div>

			<div>
				<h3 className={styles.subheading}>
					<Trans>Language Support</Trans>
				</h3>
				<div className={styles.grid}>
					{fontSamples.map((font) => (
						<div key={font.fontFamily} className={styles.card}>
							<div className={styles.cardHeader}>
								<div className={styles.cardInfo}>
									<span className={styles.fontName}>{font.name}</span>
									<span className={styles.langCode}>{font.lang.toUpperCase()}</span>
								</div>
								<span className={styles.fontFamily}>{font.fontFamily}</span>
							</div>
							<div
								className={styles.sampleText}
								style={{
									fontFamily: font.mono ? 'var(--font-mono)' : `"${font.fontFamily}", var(--font-sans)`,
									fontSize: '16px',
									textAlign: font.rtl ? 'right' : 'left',
								}}
								lang={font.lang}
								dir={font.rtl ? 'rtl' : 'ltr'}
							>
								{font.sample}
							</div>
						</div>
					))}
				</div>
			</div>

			<div>
				<h3 className={styles.subheading}>
					<Trans>Font Weights</Trans>
				</h3>
				<div className={styles.codeGrid}>
					{weightExamples.map((example) => (
						<div key={example.weight} className={styles.weightCard}>
							<div className={styles.cardHeader}>
								<span className={styles.weightLabel}>{example.label}</span>
								<span className={styles.weightValue}>{example.weight}</span>
							</div>
							<div
								style={{
									fontWeight: example.weight,
									fontFamily: 'var(--font-sans)',
									lineHeight: 1.4,
								}}
							>
								{example.text}
							</div>
							<div
								className={styles.weightItalic}
								style={{
									fontWeight: example.weight,
									fontFamily: 'var(--font-sans)',
									fontSize: '14px',
								}}
							>
								Italic style demonstration
							</div>
						</div>
					))}
				</div>
			</div>

			<div>
				<h3 className={styles.subheading}>
					<Trans>Type Scale</Trans>
				</h3>
				<div className={styles.scaleList}>
					{scaleExamples.map((example) => (
						<div key={example.size} className={styles.scaleItem}>
							<div className={styles.scaleSize}>
								<span className={styles.fontFamily}>{example.size}</span>
							</div>
							<div className={styles.scaleLabel}>
								<span className={styles.scaleLabelText}>{example.label}</span>
							</div>
							<div
								className={styles.scaleSample}
								style={{
									fontSize: example.size,
									fontWeight: example.weight,
									fontFamily: 'var(--font-sans)',
									lineHeight: 1.3,
								}}
							>
								Typography scale demonstration
							</div>
						</div>
					))}
				</div>
			</div>

			<div>
				<h3 className={styles.subheading}>
					<Trans>Style Variations</Trans>
				</h3>
				<div className={styles.grid}>
					{contrastExamples.map((example, index) => (
						<div key={index} className={styles.weightCard}>
							<div className={styles.styleLabel}>{example.style}</div>
							<div
								className={example.style.includes('Italic') ? styles.italic : ''}
								style={{
									fontSize: example.size,
									fontWeight: example.weight,
									fontFamily: 'var(--font-sans)',
									lineHeight: 1.3,
								}}
							>
								This text demonstrates {example.style.toLowerCase()} styling
							</div>
						</div>
					))}
				</div>
			</div>

			<div>
				<h3 className={styles.subheading}>
					<Trans>Code & Monospace</Trans>
				</h3>
				<div className={styles.codeGrid}>
					<div className={styles.weightCard}>
						<div className={styles.codeTitle}>Light Code</div>
						<div
							className={styles.codeLines}
							style={{
								fontFamily: 'var(--font-mono)',
								fontWeight: 300,
							}}
						>
							<div>const example = "light";</div>
							<div>function demo() {'{'}</div>
							<div> return true;</div>
							<div>{'}'}</div>
						</div>
					</div>
					<div className={styles.weightCard}>
						<div className={styles.codeTitle}>Regular Code</div>
						<div
							className={styles.codeLines}
							style={{
								fontFamily: 'var(--font-mono)',
								fontWeight: 400,
							}}
						>
							<div>const example = "regular";</div>
							<div>function demo() {'{'}</div>
							<div> return true;</div>
							<div>{'}'}</div>
						</div>
					</div>
					<div className={styles.weightCard}>
						<div className={styles.codeTitle}>Medium Code</div>
						<div
							className={styles.codeLines}
							style={{
								fontFamily: 'var(--font-mono)',
								fontWeight: 500,
							}}
						>
							<div>const example = "medium";</div>
							<div>function demo() {'{'}</div>
							<div> return true;</div>
							<div>{'}'}</div>
						</div>
					</div>
					<div className={styles.weightCard}>
						<div className={styles.codeTitle}>Bold Code</div>
						<div
							className={styles.codeLines}
							style={{
								fontFamily: 'var(--font-mono)',
								fontWeight: 600,
							}}
						>
							<div>const example = "bold";</div>
							<div>function demo() {'{'}</div>
							<div> return true;</div>
							<div>{'}'}</div>
						</div>
					</div>
				</div>
			</div>

			<div>
				<h3 className={styles.subheading}>
					<Trans>Multilingual Content</Trans>
				</h3>
				<div className={styles.multilingualCard}>
					<div className={styles.multilingualList} style={{fontFamily: 'var(--font-sans)', lineHeight: 1.6}}>
						<div className={styles.multilingualItem}>
							<strong>English:</strong> Welcome to Fluxer's typography showcase
						</div>
						<div className={styles.multilingualItem} lang="ja">
							<strong>日本語:</strong> フラクサーのタイポグラフィショーケースへようこそ
						</div>
						<div className={styles.multilingualItem} lang="ko">
							<strong>한국어:</strong> Fluxer의 타이포그래피 쇼케이스에 오신 것을 환영합니다
						</div>
						<div className={styles.multilingualItem} lang="zh-CN">
							<strong>简体中文:</strong> 欢迎来到 Fluxer 的字体展示
						</div>
						<div className={styles.multilingualItem} lang="zh-TW">
							<strong>繁體中文:</strong> 歡迎來到 Fluxer 的字體展示
						</div>
						<div className={styles.multilingualItem} lang="ar" dir="rtl">
							<strong>العربية:</strong> مرحباً بك في عرض طباعة Fluxer
						</div>
						<div className={styles.multilingualItem} lang="he">
							<strong>עברית:</strong> ברוכים הבאים לתצוגת הטיפוגרפיה של Fluxer
						</div>
						<div className={styles.multilingualItem} lang="hi">
							<strong>हिंदी:</strong> Fluxer के टाइपोग्राफी शोकेस में आपका स्वागत है
						</div>
						<div className={styles.multilingualItem} lang="th">
							<strong>ไทย:</strong> ยินดีต้อนรับสู่การจัดแสดงพิมพ์ของ Fluxer
						</div>
					</div>
				</div>
			</div>
		</div>
	);
});
