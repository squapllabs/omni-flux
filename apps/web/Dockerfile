
FROM node:16 AS build
WORKDIR /app

COPY package*.json ./

RUN npm install

COPY . .

Run npx nx build web


FROM nginx:alpine

COPY --from=build /app/dist/apps/web /usr/share/nginx/html

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
