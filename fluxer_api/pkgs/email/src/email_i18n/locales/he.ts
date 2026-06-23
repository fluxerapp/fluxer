// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_HE_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "חשבון ה-{product_name} שלך הושבת זמנית",
		"body": "שלום {username},\n\nחשבון ה-{product_name} שלך הושבת זמנית עקב זיהוי פעילות חשודה.\n\n{reason, select,\n  null {}\n  other {סיבה: {reason}}\n}\n\nכדי לקבל בחזרה גישה לחשבון, יש לאפס את הסיסמה:\n\n{forgotUrl}\n\nלאחר איפוס הסיסמה, ניתן יהיה להתחבר שוב.\n\nאם אתם חושבים שזו טעות, צרו קשר עם צוות התמיכה שלנו.\n\nצוות הבטיחות של {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "חשבון ה-{product_name} שלכם יימחק לצמיתות",
		"body": "שלום {username},\n\nחשבון ה-{product_name} שלך נקבע למחיקה לצמיתות עקב הפרות של תנאי השירות או הנחיות הקהילה שלנו.\n\nמחיקה מתוזמנת: {deletionDate, date, full} בשעה {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {סיבה: {reason}}\n}\n\nזוהי פעולת אכיפה חמורה. נתוני החשבון שלך יימחקו לצמיתות בתאריך המתוכנן.\n\nאנא עיינו ב:\n- תנאי השירות: {termsUrl}\n- הנחיות הקהילה: {guidelinesUrl}\n\nתהליך ערעור:\nאם אתם חושבים שהחלטת אכיפה זו שגויה או בלתי מוצדקת, יש לכם 60 יום להגיש ערעור. שלחו דוא\"ל לכתובת {appeals_email} מכתובת דוא\"ל זו.\n\nבערעור:\n- הסבירו בבירור מדוע אתם חושבים שהחלטת האכיפה שגויה או בלתי מוצדקת\n- ספקו כל ראיה או הקשר רלוונטיים\n\nחבר בצוות הבטיחות של {product_name} יבדוק את ערעורכם ועשוי להשהות את המחיקה הממתינה עד לקבלת החלטה סופית.\n\nצוות הבטיחות של {product_name}"
	},
	"account_temp_banned": {
		"subject": "חשבון ה-{product_name} שלך הושעה זמנית",
		"body": "שלום {username},\n\nחשבון ה-{product_name} שלך הושעה זמנית עקב הפרה של תנאי השירות או הנחיות הקהילה שלנו.\n\nמשך ההשעיה: {durationHours, plural,\n  =1 {שעה אחת}\n  other {# שעות}\n}\nהושעה עד: {bannedUntil, date, full} בשעה {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {סיבה: {reason}}\n}\n\nבמהלך תקופה זו, לא תהיה לכם גישה לחשבון.\n\nאנא עיינו ב:\n- תנאי השירות: {termsUrl}\n- הנחיות הקהילה: {guidelinesUrl}\n\nאם אתם חושבים שהחלטת אכיפה זו שגויה או בלתי מוצדקת, אתם יכולים להגיש ערעור. שלחו דוא\"ל לכתובת {appeals_email} מכתובת דוא\"ל זו והסבירו בבירור מדוע אתם חושבים שההחלטה שגויה. אנו נבדוק את ערעורכם ונשיב עם החלטתנו.\n\nצוות הבטיחות של {product_name}"
	},
	"donation_confirmation": {
		"subject": "תודה על תרומתכם ל-{product_name}",
		"body": "שלום,\n\nתודה על תרומתכם ל-{product_name}! ה{interval, select,\n  month {תרומה החוזרת}\n  year {תרומה החוזרת}\n  other {תרומה החד-פעמית}\n} שלכם {interval, select,\n  month {הוגדרה}\n  year {הוגדרה}\n  other {עובדה}\n} בהצלחה.\n\nפרטי התרומה:\nסכום: {amount} {currency} {interval, select,\n  month {לחודש}\n  year {לשנה}\n  other {}\n}\n\nStripe ישלח לכם בקרוב קבלה נפרדת עם קובץ PDF של החשבונית. הקבלה תכלול את כל פרטי התשלום וניתן יהיה להשתמש בה למטרות מס.\n\nאתם יכולים לצפות בהיסטוריית התרומות שלכם, להוריד חשבוניות, {interval, select,\n  month {ולנהל או לבטל את המנוי}\n  year {ולנהל או לבטל את המנוי}\n  other {ולנהל תרומות עתידיות}\n} בכל עת באמצעות קישור זה:\n\n{manageUrl}\n\nהתמיכה שלכם עוזרת לשמור על פעילות {product_name}. תודה!\n\nצוות {product_name}"
	},
	"donation_magic_link": {
		"subject": "ניהול התרומות שלכם ל-{product_name}",
		"body": "שלום,\n\nלחצו על הקישור למטה כדי לגשת לפורטל התורמים שלכם:\n\n{manageUrl}\n\nבפורטל, אתם יכולים לנהל מנויים, להוריד חשבוניות ולצפות בהיסטוריית התרומות שלכם.\n\nקישור זה יפוג ב- {expiresAt, date, full} בשעה {expiresAt, time, short}.\n\nאם לא ביקשתם קישור זה, אתם יכולים להתעלם בבטחה מדוא\"ל זה.\n\nצוות {product_name}"
	},
	"dsa_report_verification": {
		"subject": "אימות האימייל שלכם עבור דוח DSA",
		"body": "שלום,\n\nהשתמשו בקוד האימות למטה כדי להגיש את דוח חוק השירותים הדיגיטליים ב-{product_name}:\n\n{code}\n\nקוד זה יפוג ב- {expiresAt, date, full} בשעה {expiresAt, time, short}.\n\nאם לא ביקשתם זאת, אתם יכולים להתעלם מדוא\"ל זה.\n\nצוות הבטיחות של {product_name}"
	},
	"email_change_new": {
		"subject": "אימות האימייל החדש שלכם ל-{product_name}",
		"body": "שלום {username},\n\nהזינו קוד זה באפליקציה כדי לאמת את האימייל החדש שלכם ב-{product_name}:\n\n{code}\n\nקוד זה יפוג ב- {expiresAt, date, full} בשעה {expiresAt, time, short}.\n\nאם לא ביקשתם זאת, אתם יכולים להתעלם מדוא\"ל זה.\n\nצוות {product_name}"
	},
	"email_change_original": {
		"subject": "אישור שינוי האימייל שלכם ל-{product_name}",
		"body": "שלום {username},\n\nקיבלנו בקשה לשינוי כתובת האימייל בחשבון ה-{product_name} שלכם.\n\nכדי לאשר שינוי זה, הזינו קוד זה באפליקציה:\n\n{code}\n\nקוד זה יפוג ב- {expiresAt, date, full} בשעה {expiresAt, time, short}.\n\nאם לא ביקשתם זאת, אנא אבטחו את חשבונכם מיד.\n\nצוות {product_name}"
	},
	"email_change_revert": {
		"subject": "האימייל שלכם ב-{product_name} שונה",
		"body": "שלום {username},\n\nכתובת האימייל בחשבון ה-{product_name} שלכם שונתה ל- {newEmail}.\n\nאם אתם ביצעתם שינוי זה, אין צורך בפעולה. אם לא, אתם יכולים לבטל את השינוי ולאבטח את חשבונכם באמצעות קישור זה:\n\n{revertUrl}\n\nפעולה זו תשחזר את האימייל הקודם שלכם, תנתק אתכם מכל המכשירים, תסיר מספרי טלפון מקושרים, תבטל אימות רב-שלבי (MFA), ותדרוש מכם להגדיר סיסמה חדשה.\n\nצוות הבטיחות של {product_name}"
	},
	"email_verification": {
		"subject": "אימות האימייל שלכם ב-{product_name}",
		"body": "שלום {username},\n\nאנא אמת/י את כתובת האימייל עבור חשבון ה-{product_name} שלך על ידי לחיצה על הקישור למטה:\n\n{verifyUrl}\n\nאם לא יצרתם חשבון {product_name}, אתם יכולים להתעלם בבטחה מדוא\"ל זה.\n\nקישור זה תקף למשך 24 שעות.\n\nצוות {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "הטבות מהמתנה שמימשתם הוסרו",
		"body": "שלום {username},\n\nקוד מתנה שמימשתם שולם במקור על ידי מישהו אחר. תשלום זה בוטל מאז (החזר חיוב).\n\nבשל כך, הסרנו את ההטבות שנוספו לחשבונכם כאשר מימשתם את המתנה.\n\nאם אתם חושבים שזו טעות, אנא צרו קשר עם צוות התמיכה שלנו וכללו כל פרט שיש לכם לגבי קוד המתנה ומתי מימשתם אותו.\n\nצוות {product_name}"
	},
	"harvest_completed": {
		"subject": "ייצוא הנתונים שלכם מ-{product_name} מוכן להורדה",
		"body": "שלום {username},\n\nייצוא הנתונים שלכם מוכן.\n\nקישור להורדה:\n{downloadUrl}\n\nהודעות כלולות: {totalMessages, number}\nגודל קובץ: {fileSizeMB, number} מגה-בייט\n\nקישור זה יפוג ב- {expiresAt, date, full} בשעה {expiresAt, time, short}.\n\nאם לא ביקשתם ייצוא זה, אנא שנו את הסיסמה שלכם מיד וצרו קשר עם צוות התמיכה שלנו.\n\nצוות {product_name}"
	},
	"inactivity_warning": {
		"subject": "חשבון ה-{product_name} שלכם יימחק עקב חוסר פעילות",
		"body": "שלום {username},\n\nלא ראינו פעילות בחשבון ה-{product_name} שלכם מאז {lastActiveDate, date, full}.\n\nאם לא תתחברו עד {deletionDate, date, full} בשעה {deletionDate, time, short}, חשבונכם יימחק לצמיתות עקב חוסר פעילות.\n\nהתחברו כאן:\n{loginUrl}\n\nאם השתמשתם ב-{product_name} לאחרונה, אנא צרו קשר עם צוות התמיכה שלנו מיד.\n\nצוות {product_name}"
	},
	"ip_authorization": {
		"subject": "אישור כניסה מכתובת IP חדשה",
		"body": "שלום {username},\n\nזיהינו ניסיון התחברות לחשבון ה-{product_name} שלכם מכתובת IP חדשה:\n\nכתובת IP: {ipAddress}\nמיקום: {location}\n\nאם זה הייתם אתם, אנא אשרו כתובת IP זו על ידי לחיצה על הקישור למטה:\n\n{authUrl}\n\nאם לא ניסיתם להתחבר, אנא שנו את הסיסמה שלכם מיד.\n\nקישור זה תקף למשך 30 דקות.\n\nצוות {product_name}"
	},
	"password_change_verification": {
		"subject": "אישור שינוי הסיסמה שלכם ב-{product_name}",
		"body": "שלום {username},\n\nקיבלנו בקשה לשינוי הסיסמה בחשבון ה-{product_name} שלכם.\n\nכדי לאשר שינוי זה, הזינו קוד זה באפליקציה:\n\n{code}\n\nקוד זה יפוג ב- {expiresAt}.\n\nאם לא ביקשתם זאת, ייתכן שלמישהו יש גישה לחשבונכם. שנו את הסיסמה שלכם מיד ואפשרו אימות דו-שלבי.\n\nצוות {product_name}"
	},
	"password_reset": {
		"subject": "איפוס הסיסמה שלכם ל-{product_name}",
		"body": "שלום {username},\n\nביקשתם איפוס סיסמה ל-{product_name}. השתמשו בקישור למטה כדי להגדיר סיסמה חדשה:\n\n{resetUrl}\n\nאם לא ביקשתם זאת, אתם יכולים להתעלם בבטחה מדוא\"ל זה.\n\nקישור זה תקף למשך שעה אחת.\n\nצוות {product_name}"
	},
	"registration_approved": {
		"subject": "ההרשמה שלכם ל-{product_name} אושרה",
		"body": "שלום {username},\n\nחדשות טובות: ההרשמה שלכם ל-{product_name} אושרה.\n\nכעת אתם יכולים להתחבר לאפליקציית {product_name} כאן:\n{channelsUrl}\n\nברוכים הבאים לקהילת {product_name}.\n\nצוות {product_name}"
	},
	"report_resolved": {
		"subject": "הדוח שלכם ב-{product_name} נבדק",
		"body": "שלום {username},\n\nהדוח שלכם (מזהה: {reportId}) נבדק על ידי צוות הבטיחות שלנו.{hasComment, select, yes {\n\nתגובה מצוות הבטיחות:\n{publicComment}} other {}}\n\nתודה שעזרתם לשמור על {product_name} בטוחה לכולם. אנו מתייחסים לכל הדיווחים ברצינות ומעריכים את תרומתכם לקהילה.\n\nאם יש לכם שאלות או חששות לגבי תוצאה זו, אנא צרו קשר עם {safety_email}.\n\nצוות הבטיחות של {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "חשבון ה-{product_name} שלכם יימחק לצמיתות",
		"body": "שלום {username},\n\nחשבון ה-{product_name} שלכם נקבע למחיקה לצמיתות.\n\nמחיקה מתוזמנת: {deletionDate, date, full} בשעה {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {סיבה: {reason}}\n}\n\nזוהי פעולת אכיפה חמורה. נתוני החשבון שלכם יימחקו לצמיתות בתאריך המתוכנן.\n\nאם אתם חושבים שהחלטת אכיפה זו שגויה, אתם יכולים להגיש ערעור. שלחו דוא\"ל לכתובת {appeals_email} מכתובת דוא\"ל זו.\n\nצוות הבטיחות של {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "מחיקת חשבון ה-{product_name} שלכם מתוזמנת",
		"body": "שלום {username},\n\nביקשתם למחוק את חשבון ה-{product_name} שלכם. חשבונכם מתוזמן למחיקה לצמיתות ב:\n\n{deletionDate, date, full} בשעה {deletionDate, time, short}\n\nאם לא ביקשתם זאת, התחברו לחשבונכם כדי לבטל את המחיקה. אנו ממליצים גם לשנות את הסיסמה שלכם כדי לאבטח את החשבון.\n\nצוות {product_name}"
	},
	"unban_notification": {
		"subject": "השעיית חשבון ה-{product_name} שלכם הוסרה",
		"body": "שלום {username},\n\nחדשות טובות: השעיית חשבון ה-{product_name} שלכם הוסרה.\n\n{reason, select,\n  null {}\n  other {סיבה: {reason}}\n}\n\nכעת אתם יכולים להתחבר שוב ולהמשיך להשתמש ב-{product_name} כרגיל.\n\nצוות הבטיחות של {product_name}"
	}
});

export default EMAIL_I18N_HE_MESSAGES;
