FROM node:16

# Create app directory
WORKDIR /app

COPY package*.json ./

RUN npm install
# If you are building your code for production
# RUN npm ci --only=production

# Bundle app source
COPY . .

# RUN npx nx build api
# RUN npx nx build web

WORKDIR /app/dist/apps/api

EXPOSE 8080
CMD npx nx build api
WORKDIR /app/dist/apps/web
EXPOSE 3000
CMD RUN npx nx build web
