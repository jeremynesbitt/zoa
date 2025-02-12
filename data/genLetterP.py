from PIL import Image, ImageDraw, ImageFont

# Set up the image size, background color, and text properties
width, height = 64, 64
background_color = (255, 255, 255)  # White background
text_color = (0, 0, 0)  # Black text

# Create an image object
img = Image.new('RGB', (width, height), background_color)
draw = ImageDraw.Draw(img)

# Load a font (if you don't have a font file, you can use a built-in one)
try:
    font = ImageFont.truetype("Keyboard.ttf", 250)  # Use a large font size
except IOError:
    font = ImageFont.load_default()

fontsize = 1
text = "P"
font = ImageFont.truetype("Keyboard.ttf", fontsize)
while font.font.getsize(text)[0][1] < 50.0:
    # iterate until the text size is just larger than the criteria
    fontsize += 1
    font = ImageFont.truetype("Keyboard.ttf", fontsize)

# Get the width and height of the text to center it
# text = "P"
#text_width = draw.textlength(text, font=font)
# text_height = 20.0
(text_width, text_height), (offset_x, offset_y) = font.font.getsize(text)
print("text_height is " + str(text_height))
print("text_width is "  + str(text_width))
# text_width, text_height = draw.textsize(text, font=font)

# Calculate the position to center the text
x = (width - text_width) / 2
#y = (height - text_height) / 2
y = (text_height - height) / 2

# Add text to the image
draw.text((x, y), text, font=font, fill=text_color)

# Save the image to a file
img.save("letter_p.png")

print("Image saved as 'letter_p.png'")

